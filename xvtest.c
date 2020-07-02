
/* -------------------------------------------
 * ---		  XV Testcode		   ---
 * ---		    by AW		   ---*/


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include <stdint.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xv.h>
#include <X11/extensions/Xvlib.h>
#include <X11/extensions/XShm.h>

struct yuv_t {
    uint8_t y, u, v;
};

struct rgb_t {
    uint8_t r, g, b;
};

extern int 	XShmQueryExtension(Display*);
extern int 	XShmGetEventBase(Display*);
extern XvImage  *XvShmCreateImage(Display*, XvPortID, int, char*, int, int, XShmSegmentInfo*);

void rgb2yuv(struct rgb_t *rgb_in, struct yuv_t *yuv_out)
{
    yuv_out->y =  16 + ( 0.256f * rgb_in->r) + (0.504f * rgb_in->g) + (0.097f * rgb_in->b);
    yuv_out->u = 128 + (-0.148f * rgb_in->r) - (0.291f * rgb_in->g) + (0.439f * rgb_in->b);
    yuv_out->v = 128 + ( 0.439f * rgb_in->r) - (0.368f * rgb_in->g) - (0.071f * rgb_in->b);
    
    //printf("RGB:%d,%d,%d  YUV:%d,%d,%d\n", rgb_in->r, rgb_in->g, rgb_in->b, yuv_out->y, yuv_out->u, yuv_out->v);
}

void plot_pixel_yuv(XvImage *img, int x, int y, struct yuv_t *yuv_in)
{
    int uv_base = img->width * img->height;
    
    img->data[(img->width * y) + x] = yuv_in->y; 
    
    if(x & 1) {
        img->data[uv_base + ((img->width * y) / 2) + (x / 2) + 0] = yuv_in->u;  
    } else {
        img->data[uv_base + ((img->width * y) / 2) + (x / 2) + 1] = yuv_in->v;  
    }
}

int main (int argc, char* argv[]) {
  int		yuv_width = 1800;
  int		yuv_height = 1000;
  
  int		xv_port = -1;
  int		adaptor, encodings, attributes, formats;
  int		i, j, ret, p, _d, _w, _h;
  long		secsb, secsa, frames;
  
  XvAdaptorInfo		*ai;
  XvEncodingInfo	*ei;
  XvAttribute		*at;
  XvImageFormatValues	*fo;

  XvImage		*yuv_image;

#define GUID_YUV12_PLANAR 0x32315659

  unsigned int		p_version, p_release,
  			p_request_base, p_event_base, p_error_base;
  int			p_num_adaptors;
   	
  Display		*dpy;
  Window		window, _dw;
  XSizeHints		hint;
  XSetWindowAttributes	xswa;
  XVisualInfo		vinfo;
  int			screen;
  unsigned long		mask;
  XEvent		event;
  GC			gc;
  
  int           num = 0;

  /** for shm */
  int 			shmem_flag = 0;
  XShmSegmentInfo	yuv_shminfo;
  int			CompletionType;
  
  int p_num_formats;
  XvImageFormatValues *img_fmts;
  
  struct yuv_t yuv_col;
  yuv_col.y = 255;
  yuv_col.u = 127;
  yuv_col.v = 127;
  
  struct rgb_t rgb_col;
  
  printf("starting up video testapp...\n\n");
  
  adaptor = -1;
	
  dpy = XOpenDisplay(NULL);
  if (dpy == NULL) {
    printf("Cannot open Display.\n");
    exit (-1);
  }
  
  screen = DefaultScreen(dpy);
  
  /** find best display */
  if (XMatchVisualInfo(dpy, screen, 24, TrueColor, &vinfo)) {
    printf(" found 24bit TrueColor\n");
  } else
    if (XMatchVisualInfo(dpy, screen, 16, TrueColor, &vinfo)) {
      printf(" found 16bit TrueColor\n");
    } else
      if (XMatchVisualInfo(dpy, screen, 15, TrueColor, &vinfo)) {
	printf(" found 15bit TrueColor\n");
      } else
  	if (XMatchVisualInfo(dpy, screen, 8, PseudoColor, &vinfo)) {
	  printf(" found 8bit PseudoColor\n");
  	} else
	  if (XMatchVisualInfo(dpy, screen, 8, GrayScale, &vinfo)) {
	    printf(" found 8bit GrayScale\n");
	  } else
	    if (XMatchVisualInfo(dpy, screen, 8, StaticGray, &vinfo)) {
	      printf(" found 8bit StaticGray\n");
	    } else
	      if (XMatchVisualInfo(dpy, screen, 1, StaticGray, &vinfo)) {
  		printf(" found 1bit StaticGray\n");
	      } else {
  		printf("requires 16 bit display\n");
  		exit (-1);
	      }
  
  CompletionType = -1;	
  
  hint.x = 1;
  hint.y = 1;
  hint.width = yuv_width;
  hint.height = yuv_height;
  hint.flags = PPosition | PSize;
  
  xswa.colormap =  XCreateColormap(dpy, DefaultRootWindow(dpy), vinfo.visual, AllocNone);
  xswa.event_mask = StructureNotifyMask | ExposureMask;
  xswa.background_pixel = 0;
  xswa.border_pixel = 0;
  
  mask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask;
  
  window = XCreateWindow(dpy, DefaultRootWindow(dpy),
			 0, 0,
			 yuv_width,
			 yuv_height,
			 0, vinfo.depth,
			 InputOutput,
			 vinfo.visual,
			 mask, &xswa);
  
  XStoreName(dpy, window, "firstxv");
  XSetIconName(dpy, window, "firstxv");
  
  XSelectInput(dpy, window, StructureNotifyMask);
  
  /** Map window */
  XMapWindow(dpy, window);
  
  /** Wait for map. */
  do {
    XNextEvent(dpy, &event);
  }
  while (event.type != MapNotify || event.xmap.event != window);
  
  if (XShmQueryExtension(dpy)) shmem_flag = 1;
  if (!shmem_flag) {
    printf("no shmem available.\n");
    exit (-1);
  }
  
  if (shmem_flag==1) CompletionType = XShmGetEventBase(dpy) + ShmCompletion;
  
  
  /**--------------------------------- XV ------------------------------------*/
  printf("beginning to parse the Xvideo extension...\n\n");
  
  /** query and print Xvideo properties */
  ret = XvQueryExtension(dpy, &p_version, &p_release, &p_request_base,
			 &p_event_base, &p_error_base);
  if (ret != Success) {
    if (ret == XvBadExtension)
      printf("XvBadExtension returned at XvQueryExtension.\n");
    else
      if (ret == XvBadAlloc)
	printf("XvBadAlloc returned at XvQueryExtension.\n");
      else
	printf("other error happened at XvQueryExtension.\n");
  }
  printf("========================================\n");
  printf("XvQueryExtension returned the following:\n");
  printf("p_version      : %u\n", p_version);
  printf("p_release      : %u\n", p_release);
  printf("p_request_base : %u\n", p_request_base);
  printf("p_event_base   : %u\n", p_event_base);
  printf("p_error_base   : %u\n", p_error_base);
  printf("========================================\n");
  
  ret = XvQueryAdaptors(dpy, DefaultRootWindow(dpy),
			&p_num_adaptors, &ai);
  
  if (ret != Success) {
    if (ret == XvBadExtension)
      printf("XvBadExtension returned at XvQueryExtension.\n");
    else
      if (ret == XvBadAlloc)
	printf("XvBadAlloc returned at XvQueryExtension.\n");
      else
	printf("other error happaned at XvQueryAdaptors.\n");
  }
  printf("=======================================\n");
  printf("XvQueryAdaptors returned the following:\n");
  printf("%d adaptors available.\n", p_num_adaptors);
  for (i = 0; i < p_num_adaptors; i++) {
    printf(" name:        %s\n"
	   " type:        %s%s%s%s%s\n"
	   " ports:       %ld\n"
	   " first port:  %ld\n",
	   ai[i].name,
	   (ai[i].type & XvInputMask)	? "input | "	: "",
	   (ai[i].type & XvOutputMask)	? "output | "	: "",
	   (ai[i].type & XvVideoMask)	? "video | "	: "",
	   (ai[i].type & XvStillMask)	? "still | "	: "",
	   (ai[i].type & XvImageMask)	? "image | "	: "",
	   ai[i].num_ports,
	   ai[i].base_id);
    xv_port = ai[i].base_id;
    
    printf("adaptor %d ; format list:\n", i);
    for (j = 0; j < ai[i].num_formats; j++) {
      printf(" depth=%d, visual=%ld\n",
	     ai[i].formats[j].depth,
	     ai[i].formats[j].visual_id);
    }
    for (p = ai[i].base_id; p < ai[i].base_id+ai[i].num_ports; p++) {
      
      printf(" encoding list for port %d\n", p);
      if (XvQueryEncodings(dpy, p, &encodings, &ei) != Success) {
	printf("XvQueryEncodings failed.\n");
	continue;
      }
      for (j = 0; j < encodings; j++) {
	printf("  id=%ld, name=%s, size=%ldx%ld, numerator=%d, denominator=%d\n",
	       ei[j].encoding_id, ei[j].name, ei[j].width, ei[j].height,
	       ei[j].rate.numerator, ei[j].rate.denominator);
      }
      XvFreeEncodingInfo(ei);
      
      printf(" attribute list for port %d\n", p);
      at = XvQueryPortAttributes(dpy, p, &attributes);
      for (j = 0; j < attributes; j++) {
        printf("  name:       %s\n"
               "  flags:     %s%s\n"
               "  min_color:  %i\n"
               "  max_color:  %i\n",
               at[j].name,
               (at[j].flags & XvGettable) ? " get" : "",
               (at[j].flags & XvSettable) ? " set" : "",						
               at[j].min_value, at[j].max_value);
      }
      if (at)
	XFree(at);
      
      printf(" image format list for port %d\n", p);
      fo = XvListImageFormats(dpy, p, &formats);
      for (j = 0; j < formats; j++) {
        /*
        printf("  0x%x (%4.4s) %s\n",
               fo[j].id,
               (char *)&fo[j].id,
               (fo[j].format == XvPacked) ? "packed" : "planar");
        */
        
        printf("    4CC: 0x%08x,  type: %d,  byte_order: %d,  bits_per_pixel: %d,  format: %d,  num_planes: %d,  depth: %d, pack:  %s,  order: %s,  ybits: %d,  ubits: %d,  vbits: %d\n", \
            fo[j].id, fo[j].type, fo[j].byte_order, fo[j].bits_per_pixel, fo[j].format, fo[j].num_planes, fo[j].depth,  \
            (fo[j].format == XvPacked) ? "packed" : "planar", fo[j].component_order, \
            fo[j].y_sample_bits, fo[j].u_sample_bits, fo[j].v_sample_bits);
      }
      if (fo)
	XFree(fo);
    }
    printf("\n");
  }
  if (p_num_adaptors > 0)
    XvFreeAdaptorInfo(ai);
  if (xv_port == -1)
    exit (0);

  /*
  img_fmts = XvListImageFormats(dpy, xv_port, &p_num_formats);
  
  printf("XvListImageFormats reports %d formats\n", p_num_formats);
  
  for(i = 0; i < p_num_formats; i++) {
    printf("4CC: 0x%08x,  type: %d,  byte_order: %d,  bits_per_pixel: %d,  format: %d,  num_planes: %d,  depth: %d\n", \
        img_fmts[i].id, img_fmts[i].type, img_fmts[i].byte_order, img_fmts[i].bits_per_pixel, img_fmts[i].format, img_fmts[i].num_planes, img_fmts[i].depth);
  }
  
  printf("\n\n");
  */
  
  gc = XCreateGC(dpy, window, 0, 0);		
  
  yuv_image = XvShmCreateImage(dpy, xv_port, GUID_YUV12_PLANAR, 0, yuv_width, yuv_height, &yuv_shminfo);
  yuv_shminfo.shmid = shmget(IPC_PRIVATE, yuv_image->data_size, IPC_CREAT | 0777);
  yuv_shminfo.shmaddr = yuv_image->data = shmat(yuv_shminfo.shmid, 0, 0);
  yuv_shminfo.readOnly = False;
  
  if (!XShmAttach(dpy, &yuv_shminfo)) {
    printf("XShmAttach failed !\n");
    exit (-1);
  }
  
  printf("%d\n", yuv_image->data_size);
  
  while (1) {
    //frames = secsa = secsb = 0;
    //time(&secsa);
    
    /*
    for(i = 0; i < 255; i++) {
      rgb_col.r = i;
      rgb_col.g = i;
      rgb_col.b = i;
      rgb2yuv(&rgb_col, &yuv_col);
    }
    
    exit(-1) ;
    */
    
    for (i = 0; i < yuv_image->height; i += 1) {
      for (j = 0; j < yuv_image->width; j += 1) {
        rgb_col.r = 0;
        rgb_col.g = 0;
        rgb_col.b = i;
        rgb2yuv(&rgb_col, &yuv_col);
        
        //yuv_col.y = num;
        //yuv_col.u = i;
        //yuv_col.v = j;
        plot_pixel_yuv(yuv_image, j, i, &yuv_col);
        //yuv_image->data[yuv_image->width*i + j] = i + num;  
        //yuv_image->data[(yuv_image->width*yuv_image->height) + ((yuv_image->width*i) / 2) + (j / 2)] = j + num;  
      }
    }
    
    num += 1;
    XGetGeometry(dpy, window, &_dw, &_d, &_d, &_w, &_h, &_d, &_d);
    
    XvShmPutImage(dpy, xv_port, window, gc, yuv_image,
	   0, 0, yuv_image->width, yuv_image->height,
	   0, 0, _w, _h, True);
      
    /* XFlush(dpy); */
     
    //num++;
    printf("num=%d\n", num & 0xff);
      
    //time(&secsb);
    //printf("%ld frames in %ld seconds; %.4f fps\n", frames, secsb-secsa, (double) frames/(secsb-secsa));
  }
  
  return 0;
}
