/*
 * This file is part of YAOS and is licenced under the MIT Licence.
 *
 * armwave: an ARM-optimised waveform rendering engine for the Raspberry Pi 3.  
 * This library attempts to use NEON tricks and architectural features of the Pi's
 * processor to render waveforms damn quickly.
 */

#define _GNU_SOURCE

#ifndef NO_PYTHON
#include <Python.h>
#endif

#include <sys/ipc.h>
#include <sys/shm.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xv.h>
#include <X11/extensions/Xvlib.h>
#include <X11/extensions/XShm.h>

#include "armwave.h"

#define ARMWAVE_VER  "v0.0.1"

struct armwave_state_t g_armwave_state;
struct armwave_yuv_t yuv_lut[256];

const struct armwave_rgb_t fill_black = { 0, 0, 0 };

struct MwmHints {
    unsigned long flags;
    unsigned long functions;
    unsigned long decorations;
    long input_mode;
    unsigned long status;
};

enum {
    MWM_HINTS_FUNCTIONS = (1L << 0),
    MWM_HINTS_DECORATIONS =  (1L << 1),

    MWM_FUNC_ALL = (1L << 0),
    MWM_FUNC_RESIZE = (1L << 1),
    MWM_FUNC_MOVE = (1L << 2),
    MWM_FUNC_MINIMIZE = (1L << 3),
    MWM_FUNC_MAXIMIZE = (1L << 4),
    MWM_FUNC_CLOSE = (1L << 5)
};

/*
 * Helper function to convert 8-bit RGB to 8-bit YUV values.
 */
void rgb2yuv(struct armwave_rgb_t *rgb_in, struct armwave_yuv_t *yuv_out)
{
    yuv_out->y =  16 + ( 0.256f * rgb_in->r) + (0.504f * rgb_in->g) + (0.097f * rgb_in->b);
    yuv_out->u = 128 + (-0.148f * rgb_in->r) - (0.291f * rgb_in->g) + (0.439f * rgb_in->b);
    yuv_out->v = 128 + ( 0.439f * rgb_in->r) - (0.368f * rgb_in->g) - (0.071f * rgb_in->b);
}

/*
 * Demo/helper function to plot YUV pixel on XvImage canvas.
 */
void __attribute__((always_inline)) plot_pixel_yuv(XvImage *img, int x, int y, struct armwave_yuv_t *yuv_in)
{
    int uv_base = img->width * img->height;
    
    img->data[(img->width * y) + x] = yuv_in->y; 
    img->data[img->offsets[1] + (img->pitches[1] * (y / 2)) + (x / 2)] = yuv_in->v;
    img->data[img->offsets[2] + (img->pitches[2] * (y / 2)) + (x / 2)] = yuv_in->u;
}

/*
 * Fill an XvImage canvas with an RGB value.
 */
void fill_rgb_xvimage(XvImage *img, struct armwave_rgb_t *rgb)
{
    struct armwave_yuv_t yuv;
    
    // Compute the Y, U and V values, then use memset to block write them
    rgb2yuv(rgb, &yuv);
    
    memset(img->data + img->offsets[0], yuv.y, img->width * img->height);
    memset(img->data + img->offsets[1], yuv.v, (img->pitches[1] * img->height) / 2);
    memset(img->data + img->offsets[2], yuv.u, (img->pitches[2] * img->height) / 2);
}

/*
 * Draw a horizontal line quickly.  Takes advantage of 32-bit writes where possible.
 *
 * Colour should be passed as YUV to reduce calculation overhead.  x0 must be less than x1
 * (behaviour is undefined if this is not the case.)
 */
void draw_horiz_line_fast_xvimage(XvImage *img, int x0, int x1, int y, struct armwave_yuv_t *yuv)
{
    int length;
    uint32_t *data_y, *data_u, *data_v, yword, uword, vword;
    
    // Write pixels until x0 becomes multiple of 4
    while(x0 & 3) {
        plot_pixel_yuv(img, x0, y, yuv);
        printf("1:%d,%d\n", x0, y);
        x0++;
    }
    
    printf("2:%d,%d\n", x0, y);
        
    // Write the bulk of pixels using a loop writing 32 bits at a time.
    data_y = (uint32_t*)(img->data +(img->width * y) + x0);
    data_v = (uint32_t*)(img->data + img->offsets[1] + (img->pitches[1] * (y / 2)) + (x0 / 2));
    data_u = (uint32_t*)(img->data + img->offsets[2] + (img->pitches[2] * (y / 2)) + (x0 / 2));
    
    yword = yuv->y * 0x01010101;
    uword = yuv->u * 0x01010101;
    vword = yuv->v * 0x01010101;
    
    for(length = (x1 - x0) / 2; length > 4; length -= 4, x0 += 8) {
        printf("3:%d,%d\n", x0, y);
        *data_y++ = yword;
        *data_y++ = yword;
        *data_u++ = uword;
        *data_v++ = vword;
    }
    
    /*
    for(length = (x1 - x0) / 2; length > 4; length -= 4, x0 += 4) {
        printf("4:%d,%d\n", x0, y);
        //*data_u++ = uword;
        //*data_v++ = vword;
    }
    */
    
    // Write remaining pixels until length is zero
    while(length > 0) {
        printf("5:%d,%d\n", x0, y);
        plot_pixel_yuv(img, x0, y, yuv);
        x0++;
        length--;
    }
}

/*
 * Fast horizontal line drawing function that supports RGB.
 */
void draw_horiz_line_fast_rgb_xvimage(XvImage *img, int x0, int x1, int y, struct armwave_rgb_t *rgb)
{
    struct armwave_yuv_t yuv;
    
    rgb2yuv(rgb, &yuv);
    draw_horiz_line_fast_xvimage(img, x0, x1, y, &yuv);
}

/*
 * Prepare the YUV table for a given range of intensities.
 *
 * This can be used to generate different palettes. Right now only
 * the default palette '0' is supported which is linear intensity with
 * given trace colour.
 */
void armwave_prep_yuv_palette(int palette, struct armwave_color_mix_t *color0, struct armwave_color_mix_t *color1)
{
    int v;
    struct armwave_rgb_t temp;
    
    switch(palette) {
        case 0:
            for(v = 0; v < 256; v++) {
                temp.r = MIN((color0->r * v) >> 8, 255);
                temp.g = MIN((color0->g * v) >> 8, 255);
                temp.b = MIN((color0->b * v) >> 8, 255);
                printf("%3d = [%3d, %3d, %3d]\n", v, temp.r, temp.g, temp.b);
                rgb2yuv(&temp, &yuv_lut[v]); 
            }
            break;
    }
    
    for(v = 0; v < 256; v++) {
        printf("%3d = (%3d, %3d, %3d)\n", v, yuv_lut[v].y, yuv_lut[v].u, yuv_lut[v].v);
    }
}

/*
 * Initialise things.  STUB, mostly.
 */
void armwave_init()
{
    g_armwave_state.flags = 0;

    printf("armwave version: %s\n", ARMWAVE_VER);
    
#ifndef NO_PYTHON
    printf("built without Python linkings\n");
#endif
}

/*
 * 1ch renderer, renders up to slice-height buffer with X-coord of each waveaccess
 * pre-computed.
 *
 * @param    slice_y        Slice to render at
 * @param    height         Number of wave points to render (adjust for performance, 
 *                          accounting for cache behaviour, etc.)
 */
void render_nonaa_to_buffer_1ch_slice(uint32_t slice_y, uint32_t height)
{
    int yy, ys, w, scale_value;
    uint32_t value, word;
    uint8_t *wave_base;
    bufftyp_t *write_buffer_base;
    bufftyp_t *write_buffer;

    //write_buffer_base = g_armwave_state.ch1_buffer + (slice_y * g_armwave_state.bitdepth_height);
    write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);

    //printf("wb=0x%08x b=0x%08x ch1=0x%08x off=%d slice_y=%d height=%d scale=%d bitdepth_height=%d\n", \
        g_armwave_state.wave_buffer, write_buffer_base, g_armwave_state.ch1_buffer, \
        write_buffer_base - g_armwave_state.ch1_buffer, slice_y, height, g_armwave_state.cmp_x_bitdepth_scale, \
        g_armwave_state.bitdepth_height);

    // roll through each waveform
    for(w = 0; w < g_armwave_state.waves; w++) {
        wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);

        // roll through y and render the slice into the out buffer
        // buffer is rendered rotated by 90 degrees
        for(yy = 0; yy < height; yy += 4) {
            word = *(uint32_t*)(wave_base + yy);

            //printf("w=%d stride=%d sly=%d wave_base=0x%08x yy=%d word=0x%08x\n", w, g_armwave_state.wave_stride, slice_y, wave_base, yy, word);

            for(ys = 0; ys < 4; ys++) {
                scale_value = word & 0xff;
                
                // prevents saturating behaviour; we lose two ADC counts.
                // BUG:  waves 3/4/5 seem to be all zeroes; this kinda ignores them but obviously not a fix, what's going on?
                if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
                    continue;

                // Keep math in integer where possible.  We compute the X scale and then multiply to get the correct 
                // base coordinate.  The value of the point then informs us where to write in typically an 8-bit window.
                // The bonus of this method is that we tend to hit accesses along a 256 byte line.  (512 byte lines if
                // we set our accumulation buffer to 16 bits.)
                write_buffer = write_buffer_base + \
                    ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);

                //printf("write_buff=0x%08x value=%d\n", write_buffer, value);

                *(write_buffer + scale_value) += 1;
                word >>= 8;
            }
        }
    }

    //printf("wb_end=%d\n", write_buffer - write_buffer_base);
}

/*
 * Render buffer to an XvImage canvas.
 */
void armwave_fill_xvimage_scaled(XvImage *img)
{
    uint32_t xx, yy, ye, y, word, wave_word, painted = 0;
    // uint32_t ysub;
    int rr, gg, bb, n, nsub, npix, w;
    uint8_t r, g, b;
    int value; 
    // uint8_t row;
    uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
    //uint32_t *out_buffer_base = out_buffer;
    uint32_t offset;

    //printf("output buffer: 0x%08x\n", out_buffer);

    npix = g_armwave_state.target_width * g_armwave_state.bitdepth_height; 
    
    //printf("memset %d bytes, npix %d, chbuff sz %d, base32_ptr 0x%08x, dest_buffer 0x%08x, stack ~0x%08x\n", \
        g_armwave_state.target_width * g_armwave_state.target_height * 4, npix, \
        g_armwave_state.ch_buff_size, base_32ptr, out_buffer_base, &w);

    // we don't really want to be doing this if possible;  os.madvise may be a better option
    //memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
    fill_rgb_xvimage(img, &fill_black);
    
    //printf("iter...\n");

    for(n = 0; n < npix; n += 2) {
        wave_word = *base_32ptr++;

        if(COND_UNLIKELY(wave_word != 0)) {
            for(w = 0; w < 2; w++) {
                value = wave_word & 0xffff;
                wave_word >>= 16;

                if(value != 0) {
                    // Plot the pixels
                    nsub = n + w;
                    yy = (nsub & 0xff); // * g_armwave_state.vscale_frac;
                    ye = ((nsub & 0xff) + 1); // * g_armwave_state.vscale_frac;
                    xx = (nsub >> 8) / 2;

                    for(y = yy; y < ye; y++) {
                        //offset = (xx + ((g_armwave_state.target_height - y) * g_armwave_state.target_width)); 
                        //printf("0x%08x,%6d,%6d,%6d,%6d,%4d,%.3f\n", out_buffer_base, offset, xx, y, n, g_armwave_state.target_width, g_armwave_state.vscale_frac);
                        //*(out_buffer_base + offset) = word;
                        //printf("%6d,%6d,%6d\n", xx, yy, value);
                        plot_pixel_yuv(img, xx, yy, &yuv_lut[MIN(value, 255)]);
                        painted++;
                    }
                }
            }
        }
    }

    //printf("...done paint %d pixels...\n", painted);
}

/*
 * Fill buffers with rendered waveform (only supports Ch1 so far.)
 */
void armwave_generate()
{
    uint32_t yy;
    uint32_t xx_rem = g_armwave_state.wave_length, ypos = 0;

    memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);

    for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
    //for(yy = 0; yy < 10; yy++) {
        //printf("armwave_generate: slice %d (y=%d, h=%d, xpos=%d)\n", \
            yy, yy * g_armwave_state.slice_height, g_armwave_state.slice_height, \
            (yy * g_armwave_state.slice_height * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT);

        render_nonaa_to_buffer_1ch_slice(yy * g_armwave_state.slice_height, g_armwave_state.slice_height);
        xx_rem -= g_armwave_state.slice_height;
        ypos += g_armwave_state.slice_height;   
    }

    /*
    printf("armwave_generate: slice %d (y=%d, h=%d, xpos=%d) last\n", \
            yy, ypos, g_armwave_state.slice_height, 
            (yy * g_armwave_state.slice_height * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT);
    render_nonaa_to_buffer_1ch_slice(ypos, xx_rem);
    */
}

/*
 * Setup the renderer with passed parameters.
 */
void armwave_setup_render(uint32_t start_point, uint32_t end_point, uint32_t waves_max, uint32_t wave_stride, uint32_t target_width, uint32_t target_height, uint32_t render_flags)
{
    uint32_t length, xx;
    float points_per_pixel;

    printf("s=%d e=%d w=%d ws=%d tw=%d th=%d rf=0x%08x\n", start_point, end_point, waves_max, wave_stride, target_width, target_height, render_flags);

    // TODO these asserts should instead raise PyExc
    assert(start_point < end_point);

    /*
    // target_height must be a power of two.  Only 256, 512, 1024 and 2048 height buffers are supported.
    assert(target_height == 256 || target_height == 512 || target_height == 1024 || target_height == 2048);
    if(target_height == 256) {
        g_armwave_state.row_shift = 8;
        g_armwave_state.row_mask = 0x0ff;
    } else if(target_height == 512) {
        g_armwave_state.row_shift = 9;
        g_armwave_state.row_mask = 0x1ff;
    } else if(target_height == 1024) {
        g_armwave_state.row_shift = 10;
        g_armwave_state.row_mask = 0x3ff;
    } else if(target_height == 2048) {
        g_armwave_state.row_shift = 11;
        g_armwave_state.row_mask = 0x7ff;
    }
    */

    // Calculate the size of each buffer.  Buffers are rotated by 90 degrees to improve cache coherency.
    g_armwave_state.xstride = target_height;
    g_armwave_state.vscale_frac = target_height / 255.0f;
    g_armwave_state.vscale = (int)g_armwave_state.vscale_frac;
    g_armwave_state.wave_stride = wave_stride;
    g_armwave_state.waves_max = waves_max;
    g_armwave_state.waves = waves_max;  // Need a function to be able to change this on the fly
    g_armwave_state.size = target_height * target_width;
    g_armwave_state.bitdepth_height = 256 * sizeof(bufftyp_t);  // Always 256 possible levels in 8-bit mode
    g_armwave_state.ch_buff_size = (g_armwave_state.bitdepth_height + 4) * (target_width + 4) * sizeof(bufftyp_t);  // Add word padding too
    g_armwave_state.target_width = target_width;
    g_armwave_state.target_height = target_height;
    g_armwave_state.wave_length = end_point - start_point;

    // Calculate compound scaler
    g_armwave_state.cmp_x_bitdepth_scale = \
        ((g_armwave_state.target_width * (1.0f / g_armwave_state.wave_length))) * (1 << AM_XCOORD_MULT_SHIFT);

    printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
        g_armwave_state.ch_buff_size, g_armwave_state.cmp_x_bitdepth_scale, \
        g_armwave_state.cmp_x_bitdepth_scale, g_armwave_state.target_width, g_armwave_state.wave_length,
        (1 << AM_XCOORD_MULT_SHIFT));

    // In 1ch mode, target 1024 x 16 render buffer, reading 16 bytes at a time from each wave, retaining as much as possible in L1/L2 cache
    // In 2ch mode, target two 1024 x 8 render buffers, reading 16 bytes at a time from each wave
    // In 4ch mode, target four 1024 x 4 render buffers, reading 16 bytes at a time from each wave
    g_armwave_state.slice_height = 64; // 64;  

    if(g_armwave_state.ch1_buffer != NULL)
        free(g_armwave_state.ch1_buffer);

    g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);

    if(g_armwave_state.ch1_buffer == NULL) {
        fprintf(stderr, "malloc failure allocating %d bytes (g_armwave_state.ch1_buffer)\n", g_armwave_state.ch_buff_size);
        exit(-1);
    }

    // Precompute the x-coord to pixel lookup to reduce ALU load
    length = end_point - start_point;
    points_per_pixel = length / ((float)(target_width));
    g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;

    /*
    g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));

    assert(g_armwave_state.xcoord_to_xpixel != NULL);

    for(xx = 0; xx < length; xx++) {
        g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;

        // printf("xcoord_to_xpixel[%5d] = %5d (scale:%8.3f)\n", xx, g_armwave_state.xcoord_to_xpixel[xx], 1.0f / points_per_pixel);
    }
    */

    g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);

    printf("Ptrs: 0x%08x 0x%08x 0x%08x 0x%08x \n", \
        (uint32_t)g_armwave_state.ch1_buffer, \
        (uint32_t)g_armwave_state.xcoord_to_xpixel, \
        (uint32_t)g_armwave_state.out_pixbuf, \
        (uint32_t)g_armwave_state.test_wave_buffer);

     //malloc_stats();
}

/*
 * Set wave buffer pointer.  This needs to be changed to support disjointed buffers
 * in the future.
 */
void armwave_set_wave_pointer(uint8_t *wave_buffer)
{
    assert(wave_buffer != NULL);
    g_armwave_state.wave_buffer = wave_buffer;
}

/*
 * Set the wave buffer pointer as the test waveform buffer filled by such functions
 * as `armwave_test_create_square` and `armwave_test_create_am_sine`.
 */
void armwave_set_wave_pointer_as_testbuf(int set)
{
    if(set > g_armwave_state.test_wave_buffer_nsets) {
        printf("armwave_set_wave_pointer_as_testbuf: error, nsets exceeded\n");
        return;
    }

    g_armwave_state.wave_buffer = g_armwave_state.test_wave_buffer + (g_armwave_state.test_wave_buffer_stride * set);
}

/*
 * Set wave buffer pointer from a 32-bit uint.  This needs to be changed to support 
 * disjointed buffers in the future.
 */
void armwave_set_wave_pointer_u32(uint32_t wave_buffer_ptr)
{
    assert(wave_buffer_ptr != 0);
    g_armwave_state.wave_buffer = (uint8_t*)wave_buffer_ptr;
}

/*
 * Clear the working buffer (fill it with all zeros.)
 */
void armwave_clear_buffer(uint32_t flags)
{
    // Flags ignored, only one buffer cleared
    memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
}

/*
 * Set the render colour for a channel.  R/G/B may exceed 255 for saturation effects.  
 * `I` sets intensity multiplier for all colours.
 */
void armwave_set_channel_colour(int ch, int r, int g, int b, float i)
{
    // Only 1ch supported for now
    switch(ch) {
        case 1:
            g_armwave_state.ch1_color.r = r * i;
            g_armwave_state.ch1_color.g = g * i;
            g_armwave_state.ch1_color.b = b * i;
            break;
    }
}

/*
 * Dump a ppm of a buffer to a file.
 */
void armwave_dump_ppm_debug(uint32_t *buffer, char *fn)
{
    FILE *fp = fopen(fn, "wb");
    uint32_t data;
    int xx, yy;

    //printf("in_buffer=0x%08x\n", buffer);

    fputs("P3\n", fp);
    fprintf(fp, "%d %d\n", g_armwave_state.target_width, g_armwave_state.target_height);
    fputs("255\n", fp);

    for(yy = 0; yy < g_armwave_state.target_height; yy++) {
        for(xx = 0; xx < g_armwave_state.target_width; xx++) {
            data = *(buffer + (xx + (yy * g_armwave_state.target_width)));
            //printf("xx,yy=%4d,%4d, word=0x%08x\n", xx, yy, data);

            fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
        }
    }

    fclose(fp);
}

/*
 * Initialise some test functionry.
 */
void armwave_test_init(int wave_size, int nwaves, int render_width, int render_height)
{
}

/*
 * Render image to the local allocated buffer.
 */
void armwave_test_fill_outbuf()
{
    //armwave_fill_pixbuf_scaled(g_armwave_state.out_pixbuf);
}

/*
 * Dump the working local allocated buffer to a ppm file for debug.
 */
void armwave_test_dump_buffer_to_ppm(char *fn)
{
    armwave_dump_ppm_debug(g_armwave_state.out_pixbuf, fn);
}

/*
 * Render GDK buffer with test funtionry.
 */
#ifndef NO_PYTHON
void armwave_test_fill_gdkbuf(PyObject *buf)
{
    //PyObject *mv;
    //Py_buffer *buf = malloc(sizeof(Py_buffer));

    // Holy jesus dear mother of God, what have we done?
    void *out_pixbuf = ((uint32_t ***)buf)[2][10];
    
    // TODO: use armwave_fill_pixbuf_256 for 256-height buffers for performance?
    //armwave_fill_pixbuf_scaled(out_pixbuf);
}
#endif

/*
 * Allocate a test buffer, freeing any existing buffer.
 */
void armwave_test_buffer_alloc(int nsets)
{
    if(g_armwave_state.test_wave_buffer != NULL) {
        free(g_armwave_state.test_wave_buffer);
    }

    //printf("armwave_test_buffer_alloc: length=%d max=%d\n", g_armwave_state.wave_length, g_armwave_state.waves_max);

    g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);

    if(g_armwave_state.test_wave_buffer == NULL) {
        printf("armwave_test_buffer_alloc: failed to allocate test wave buffer (%d bytes, %d sets)\n", \
            g_armwave_state.wave_length * g_armwave_state.waves_max * nsets, nsets);
        return;
    }
}

/*
 * Fill a pixbuf PyBuffer with a rendered waveform.
 */
#ifndef NO_PYTHON
PyObject *armwave_fill_pixbuf_into_pybuffer(PyObject *buf_obj)
{
    Py_buffer buffer;
    int ret;

    //printf("armwave_fill_pixbuf_into_pybuffer: start\n");

    ret = PyObject_GetBuffer(buf_obj, &buffer, PyBUF_SIMPLE | PyBUF_WRITABLE);

    if(ret != 0) {
        printf("armwave_fill_pixbuf_into_pybuffer: PyObject_GetBuffer() failed, returning PyFalse\n");
        Py_RETURN_FALSE;
    }

    //printf("PyObject_GetBuffer did not trigger assert: buffer->buf=0x%08x, buffer->obj=0x%08x, buffer->len=%d\n", buffer->buf, buffer->obj, buffer->len);

    //armwave_fill_pixbuf_scaled(buffer.buf);
    //printf("armwave_fill_pixbuf_into_pybuffer: buffer fill done\n");

    PyBuffer_Release(&buffer);
    //printf("armwave_fill_pixbuf_into_pybuffer: done\n");

    Py_RETURN_TRUE;
}
#endif

/*
 * Make a test AM waveform for render tests.
 *
 * @param   mod                     modulation depth
 * @param   noise_fraction          typically 1e-6`
 * @param   number of wave sets     1-N, must have memory for these
 */
void armwave_test_create_am_sine(float mod, float noise_fraction, int sets)
{
    float v, noise, xnoise, mod_val;
    float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
    int s, set_offset = 0;
    int w, x;

    g_armwave_state.test_wave_buffer_stride = (g_armwave_state.waves * g_armwave_state.wave_stride);
    g_armwave_state.test_wave_buffer_nsets = sets;
    armwave_test_buffer_alloc(sets);

    for(s = 0; s < sets; s++) {
        printf("Calculating test set %d\n", s);
    
        for(w = 0; w < g_armwave_state.waves; w++) {
            //mod_val = 0.5f + (((float)w / g_armwave_state.waves) * mod);
            mod_val = 0.5f * (sin((_1_waves_mod * w) * 6.28f) + 1.0f);

            for(x = 0; x < g_armwave_state.wave_length; x++) {
                noise  = ((rand() & 0xffff) * noise_fraction);
                noise *= noise;
                noise *= noise;
                noise *= noise;

                if((rand() & 0xffff) > 0x7fff)
                    noise = -noise;

                noise += 1.0f;
                xnoise = (rand() & 0xffff) / 6553500.0f;

                v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
                //v = ((x & 0xff) / 128.0f) - 1.0f;
                //printf("%d = %d\n", x + (w * g_armwave_state.wave_stride), MIN(MAX(128 + (v * 127), 0), 255));
                g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
            }
        }

        set_offset += (g_armwave_state.waves * g_armwave_state.wave_stride);
    }
}

/*
 * Make a test square waveform.
 *
 * @param   noise_fraction      typically 1e-6
 */
#if 0
void armwave_test_create_square(float noise_fraction)
{
    uint8_t v;
    float noise, xnoise;
    float level = 0.8f, new_level = 0.8f;
    int w, x;

    for(w = 0; w < g_armwave_state.waves; w++) {
        for(x = 0; x < g_armwave_state.wave_length; x++) {
            noise  = ((rand() & 0xffff) * noise_fraction);
            noise *= noise;
            noise *= noise;
            noise *= noise;

            if((rand() & 0xff) > 0x7f)
                noise = -noise;

            //noise += 1.0f;

            if(x > (g_armwave_state.wave_length * 0.75f)) {
                new_level = 0.2f;
            } else if(x > (g_armwave_state.wave_length * 0.5f)) {
                new_level = 0.8f;
            } else if(x > (g_armwave_state.wave_length * 0.25f)) {
                new_level = 0.2f;
            } else {
                new_level = 0.8f;
            }

            level = ((level * 3) + new_level) * 0.25f;

            v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
            g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = v;
        }
    }
}
#endif

/*
 * Free all buffers and set to NULL, ready to be reinitialised or stopped.
 */
void armwave_cleanup()
{
    free(g_armwave_state.out_pixbuf);
    free(g_armwave_state.ch1_buffer);
    free(g_armwave_state.xcoord_to_xpixel);
    free(g_armwave_state.test_wave_buffer);

    g_armwave_state.out_pixbuf = NULL;
    g_armwave_state.ch1_buffer = NULL;
    g_armwave_state.xcoord_to_xpixel = NULL;
    g_armwave_state.test_wave_buffer = NULL;
}

/*
 * Main entry point for the testcase.  Based on:
 * http://bellet.info/XVideo/testxv.c
 */
#ifdef NO_PYTHON
int main()
{
    int	yuv_width = 1024;
    int	yuv_height = 256;
    
    int	xv_port = -1;
    int	adaptor = -1, encodings, attributes, formats;
    int	i, j, ret, p, _d, _w, _h, n;
    long secsb, secsa, frames;
    
    XvAdaptorInfo *ai;
    XvEncodingInfo *ei;
    XvAttribute	*at;
    XvImageFormatValues	*fo;
    XvImage	*yuv_image;

    unsigned int p_version, p_release, p_request_base, p_event_base, p_error_base;
    int	p_num_adaptors;
     	
    Display	*dpy;
    Window	window, _dw;
    XSizeHints hint;
    XSetWindowAttributes xswa;
    XVisualInfo	vinfo;
    int	screen;
    unsigned long mask;
    XEvent event;
    GC gc;
    int shmem_flag = 0;
    XShmSegmentInfo	yuv_shminfo;
    int	CompletionType;
    
    int p_num_formats;
    XvImageFormatValues *img_fmts;
    
    struct armwave_yuv_t grat_col;
    struct armwave_yuv_t yuv_col;
    struct armwave_rgb_t grat_rgb_col;
    struct armwave_rgb_t rgb_col;
    int num = 0;
    
    int n_test_waves = 8;
    
    yuv_col.y = 255;
    yuv_col.u = 127;
    yuv_col.v = 127;
    
    grat_rgb_col.r = 255;
    grat_rgb_col.g = 0;
    grat_rgb_col.b = 0;
    
    rgb2yuv(&grat_rgb_col, &grat_col);
    
    printf("Starting up testapp...\n\n");
    
    /*
     * Try to open the display.
     */
    dpy = XOpenDisplay(NULL);
    if (dpy == NULL) {
        printf("Cannot open display.\n");
        exit (-1);
    }
    
    screen = DefaultScreen(dpy);
    
    /*
     * Set up the renderer.
     */
    printf("Preparing test waveforms...\n");
    armwave_setup_render(0, 1024, 256, 1024, 1024, 256, 0);
    armwave_set_channel_colour(1, 255, 178, 25, 10.0f);
    armwave_prep_yuv_palette(0, &g_armwave_state.ch1_color, &g_armwave_state.ch1_color);
    armwave_test_create_am_sine(0.25, 1e-5, n_test_waves);
    printf("Done, starting XVideo...\n");
    
    /*
     * Check the display supports 24-bit TrueColor, if not then abort early.
     */
    if (XMatchVisualInfo(dpy, screen, 24, TrueColor, &vinfo)) {
        printf("Found 24bit TrueColor.\n");
    } else {
        printf("Error: Fatal X11: not supported 24-bit TrueColor display.\n");
        exit(-1);
    }
    
    /*
     * Create the window and map it, then wait for it to send us a map event.
     */
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
    
    XStoreName(dpy, window, "ArmWave");
    XSetIconName(dpy, window, "ArmWave");
    XSelectInput(dpy, window, StructureNotifyMask);
    
    XMapWindow(dpy, window);
    
    do {
        XNextEvent(dpy, &event);
    }
    while (event.type != MapNotify || event.xmap.event != window);
    
    /*
     * Try to strip decoration from window.
     */
    /*
    Atom mwmHintsProperty = XInternAtom(dpy, "_MOTIF_WM_HINTS", 0);
    struct MwmHints hints;
    hints.flags = MWM_HINTS_DECORATIONS;
    hints.decorations = 0;
    XChangeProperty(dpy, window, mwmHintsProperty, mwmHintsProperty, 32,
            PropModeReplace, (unsigned char *)&hints, 5);
    */
    
    /*
     * Query the MITSHM extension - check it is available.
     */
    if (XShmQueryExtension(dpy)) {
        shmem_flag = 1;
    }
    
    if (!shmem_flag) {
        printf("Error: Fatal X11: Shared memory extension not available or failed to allocate shared memory.\n");
        exit(-1);
    }
    
    if (shmem_flag == 1) {
        CompletionType = XShmGetEventBase(dpy) + ShmCompletion;
    }
    
    ret = XvQueryExtension(dpy, &p_version, &p_release, &p_request_base,
			 &p_event_base, &p_error_base);
    if (ret != Success) {
        printf("Error: Fatal X11: Unable to find XVideo extension (%d).  Is it configured correctly?\n", ret);
        exit(-1);
    }
    
    ret = XvQueryAdaptors(dpy, DefaultRootWindow(dpy),
			&p_num_adaptors, &ai);
    
    if (ret != Success) {
        printf("Error: Fatal X11: Unable to query XVideo extension (%d).  Is it configured correctly?\n", ret);
        exit(-1);
    }
    
    // Use the last port available
    xv_port = ai[p_num_adaptors - 1].base_id;
    if(xv_port == -1) {
        printf("Error: Fatal X11: Unable to use the port %d\n\n", p_num_adaptors - 1);
        exit(-1);
    }
    
    gc = XCreateGC(dpy, window, 0, 0);		
    
    yuv_image = XvShmCreateImage(dpy, xv_port, GUID_YUV12_PLANAR, 0, yuv_width, yuv_height, &yuv_shminfo);
    yuv_shminfo.shmid = shmget(IPC_PRIVATE, yuv_image->data_size, IPC_CREAT | 0777);
    yuv_shminfo.shmaddr = yuv_image->data = shmat(yuv_shminfo.shmid, 0, 0);
    yuv_shminfo.readOnly = False;
    
    for(n = 0; n < yuv_image->num_planes; n++) {
        printf("yuv_image plane %d offset %d pitch %d\n", n, yuv_image->offsets[n], yuv_image->pitches[n]);
    }
    
    if (!XShmAttach(dpy, &yuv_shminfo)) {
        printf("Error: Fatal X11: XShmAttached failed\n", ret);
        exit (-1);
    }
    
    printf("%d\n", yuv_image->data_size);
    
    while (1) {
        armwave_set_wave_pointer_as_testbuf(num % n_test_waves);
        armwave_generate();
        
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
        
        armwave_fill_xvimage_scaled(yuv_image);
        
#if 1
        for(j = 16; j < (yuv_image->height - 16); j += 16) {
            printf("Render line %d\n", j);
            draw_horiz_line_fast_rgb_xvimage(yuv_image, 0, num % yuv_image->width, j, &grat_col);
        } 
#endif

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
#endif
