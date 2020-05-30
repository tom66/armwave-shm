/*
 * This file is part of YAOS and is licenced under the MIT Licence.
 *
 * armwave: an ARM-optimised waveform rendering engine for the Raspberry Pi 3.  
 * This library attempts to use NEON tricks and architectural features of the Pi's
 * processor to render waveforms damn quickly.
 */

#define _GNU_SOURCE

#include <Python.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#include "armwave.h"

#define TEST_WAVE_SIZE			2048
#define TEST_NWAVES				64

#define ARMWAVE_VER				"v0.0.1"

#define MAX(a,b)  				((a) > (b) ? (a) : (b))
#define MIN(a,b)         		((a) < (b) ? (a) : (b))

#define CLAMP(x,mi,mx)    		MIN(MAX((x),mi),mx)

struct armwave_state_t g_armwave_state;

uint8_t test_wave_buffer[TEST_WAVE_SIZE * TEST_NWAVES];
uint8_t gamma_table[256];

// for now...
const float overall_scale = 2550.0f / 255.0f;

float mod_depth = 0.0f;

/*
 * Make a test AM waveform for render tests.
 */
void test_create_waveform()
{
    float v, mod, noise, xnoise;
    int w, x;

    for(w = 0; w < TEST_NWAVES; w++) {
        mod = 0.5f + (((float)w / TEST_NWAVES) * mod_depth);
        //mod = 1.0f;

        for(x = 0; x < TEST_WAVE_SIZE; x++) {
            noise  = ((rand() & 0xffff) / 100000.0f);
            noise *= noise;
            noise *= noise;
            noise *= noise;

            if((rand() & 0xffff) > 0x7fff)
                noise = -noise;

            noise += 1.0f;
            xnoise = (rand() & 0xffff) / 6553500.0f;

            v = (sin((6.28f * x * (1.0f / TEST_WAVE_SIZE)) + xnoise) * mod) * noise;
            //v = ((x & 0xff) / 128.0f) - 1.0f;
            test_wave_buffer[x + (w * TEST_WAVE_SIZE)] = MIN(MAX(128 + (v * 127), 0), 255);
        }
    }
}

/*
 * Create a gamma table.
 */
void test_create_gamma()
{
    int i;
    float gamma = 0.90f;

    for(i = 0; i < 256; i++) {
        gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
    }
}

/*
 * 1ch renderer, renders up to slice-height buffer with X-coord of each waveaccess
 * pre-computed.
 *
 * @param    slice_y        Slice to render at
 * @param    height         Number of wave points to render
 */
void render_nonaa_to_buffer_1ch_slice(uint32_t slice_y, uint32_t height)
{
    int yy, ys, w, scale_value;
    uint32_t value, word;
    uint8_t *wave_base;
    uint8_t *write_buffer_base;
    uint8_t *write_buffer;
    
    write_buffer_base = g_armwave_state.ch1_buffer + (slice_y * g_armwave_state.target_height);

    // roll through each waveform
    for(w = 0; w < g_armwave_state.waves; w++) {
        wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);

#if 0
        printf("Rendering wave %4d, wave_base=0x%08x (offs:%8d) (%3d), buffer_base=0x%08x (offs:%8d)\n", \
            w, wave_base, wave_base - g_armwave_state.wave_buffer, *g_armwave_state.wave_buffer, \
            write_buffer_base, write_buffer_base - g_armwave_state.ch1_buffer);
#endif

        // roll through y and render the slice into the out buffer
        // buffer is rendered rotated by 90 degrees
        for(yy = 0; yy < height; yy += 4) {
            //write_buffer = write_buffer_base + (g_armwave_state.xcoord_to_xpixel[slice_y + yy] * g_armwave_state.target_width);

#if 0
            value = (*(wave_base + yy)) * g_armwave_state.vscale;

            printf("Rendering row %5d, sum-y %5d, address=0x%08x (offs:%8d), value_at_pixel=%3d, xcooord_to_xpixel=%5d, "
                   "scaled_xcoord_to_xpixel=%5d, wave_base=0x%08x\n", \
                   yy, slice_y + yy, write_buffer, write_buffer - g_armwave_state.ch1_buffer, \
                   value, g_armwave_state.xcoord_to_xpixel[slice_y + yy], \
                   g_armwave_state.xcoord_to_xpixel[slice_y + yy], \
                   wave_base + yy);
#endif

            //value = (*(wave_base + yy)) * g_armwave_state.vscale;
            word = *(uint32_t*)(wave_base + yy);
            //value = 4; // 5 * g_armwave_state.vscale;
            //*(write_buffer + value) = 0xff;

            for(ys = 0; ys < 4; ys++) {
            	scale_value = (word & 0xff) * g_armwave_state.vscale;
            	write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.target_height);
            	*(write_buffer + scale_value) += 1;
            	word >>= 8;
    		}

            //write_buffer_base += g_armwave_state.target_width;
        }
    }
}

void armwave_init()
{
    g_armwave_state.flags = 0;
}

void armwave_setup_render(uint8_t *wave_buffer, uint32_t start_point, uint32_t end_point, uint32_t waves, uint32_t wave_stride, uint32_t target_width, uint32_t target_height, uint32_t render_flags)
{
    uint32_t length, xx;
    float points_per_pixel;

    assert(start_point < end_point);

    // Pretend we're in 1ch, 8-bit mode only for now
    g_armwave_state.wave_buffer = wave_buffer;

    // target_height must be multiple of 256 (8-bit samples);  other sizes should be scaled somehow
    assert((target_height % 256) == 0);

    // Calculate the size of each buffer.  Buffers are rotated by 90 degrees to improve cache coherency.
    g_armwave_state.xstride = target_height;
    g_armwave_state.vscale = target_height / 256;
    g_armwave_state.wave_stride = wave_stride;
    g_armwave_state.waves = waves;
    g_armwave_state.size = target_height * target_width;
    g_armwave_state.target_width = target_width;
    g_armwave_state.target_height = target_height;

    // In 1ch mode, target 1024 x 16 render buffer, reading 16 bytes at a time from each wave, retaining as much as possible in L1/L2 cache
    // In 2ch mode, target two 1024 x 8 render buffers, reading 16 bytes at a time from each wave
    // In 4ch mode, target four 1024 x 4 render buffers, reading 16 bytes at a time from each wave
    g_armwave_state.slice_height = 64;  

    if(g_armwave_state.ch1_buffer != NULL)
        free(g_armwave_state.ch1_buffer);

    g_armwave_state.ch1_buffer = calloc(g_armwave_state.size, 1);
    g_armwave_state.ch1_color.r = 255 * overall_scale;
    g_armwave_state.ch1_color.g = 178 * overall_scale;
    g_armwave_state.ch1_color.b = 25 * overall_scale;

    assert(g_armwave_state.ch1_buffer != NULL);

    // Precompute the x-coord to pixel lookup to reduce ALU load
    length = end_point - start_point;
    points_per_pixel = length / ((float)(target_width));
    g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
    g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));

    assert(g_armwave_state.xcoord_to_xpixel != NULL);

    for(xx = 0; xx < length; xx++) {
        g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;

        // printf("xcoord_to_xpixel[%5d] = %5d (scale:%8.3f)\n", xx, g_armwave_state.xcoord_to_xpixel[xx], 1.0f / points_per_pixel);
    }

    g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
}

void armwave_clear_buffer(uint32_t flags)
{
    // Flags ignored, only one buffer cleared
    memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.size);
}

/*
void armwave_fill_pixbuf(uint32_t *out_buffer)
{
    uint32_t xx, yy, addr, value, word;
    int rr, gg, bb;
    uint8_t r, g, b;
    uint8_t *base_ptr = g_armwave_state.ch1_buffer;
    uint32_t *out_buffer_base = out_buffer;

    assert(out_buffer != NULL);
    //printf("out_buffer=0x%08x\n", out_buffer);
    //printf("out_buffer_size=%d\n", sizeof(uint32_t) * g_armwave_state.size);

#if 0
    // Pixbuf tests
    for(xx = 0; xx < g_armwave_state.target_width; xx++) {
        for(yy = 0; yy < g_armwave_state.target_height; yy++) {
            //printf("xx=%d, yy=%d\n", xx, yy);
            *(out_buffer + ((xx + (yy * g_armwave_state.target_width)) / 4)) = (yy / 4) | (((yy / 4)) << 8) | (((yy / 4)) << 16);
        }
    }
#endif

#if 1
    // Buffer is sent non-rotated: we use GDK/GL to assemble and rotate it
    for(yy = 0; yy < g_armwave_state.target_height; yy++) {
        for(xx = 0; xx < g_armwave_state.target_width; xx++) {
            //printf("xx,yy=%d,%d, row_ptr=0x%08x\n", xx, yy, row_ptr);
            value = *(base_ptr + xx + (yy * g_armwave_state.target_width));
            //value = xx / 8; // *(row_ptr + xx);
            //printf("xx,yy=%d,%d, value=%d\n", xx, yy, value);

#if 0
            //rr = gamma_table[(uint8_t)(g_armwave_state.ch1_color.r * value)];  // We could also do a gamma LUT here
            //gg = gamma_table[(uint8_t)(g_armwave_state.ch1_color.g * value)];
            //bb = gamma_table[(uint8_t)(g_armwave_state.ch1_color.b * value)];

            //rr = CLAMP(rr * overall_scale, 0, 255);
            //gg = CLAMP(gg * overall_scale, 0, 255);
            //bb = CLAMP(bb * overall_scale, 0, 255);
#endif

            if(value != 0) {
	            rr = (g_armwave_state.ch1_color.r * value) >> 8;
	            gg = (g_armwave_state.ch1_color.g * value) >> 8;
	            bb = (g_armwave_state.ch1_color.b * value) >> 8;

	            r = MIN(rr, 255);
	            g = MIN(gg, 255);
	            b = MIN(bb, 255);

	            // ensure 100% alpha channel, if it is used
	            word = 0xff000000 | (b << 16) | (g << 8) | r;

	            //printf("xx,yy=%4d,%4d, value=%3d, word=0x%08x, rr=%3d, gg=%3d, bb=%3d\n", xx, yy, value, word, rr, gg, bb);

	            *out_buffer++ = word;
	        }
        }
    }
#endif
}
*/

void armwave_fill_pixbuf2(uint32_t *out_buffer)
{
    uint32_t xx, yy, addr, value, word;
    int rr, gg, bb;
    uint8_t r, g, b;
    uint8_t *base_ptr = g_armwave_state.ch1_buffer;
    uint32_t *out_buffer_base = out_buffer;
    uint32_t npix, n;

    assert(out_buffer != NULL);

    // Buffer is sent non-rotated: we use GDK/GL to assemble and rotate it

    npix = g_armwave_state.target_width * g_armwave_state.target_height;

    for(n = 0; n < npix; n++) {
        //value = *(base_ptr + xx + (yy * g_armwave_state.target_width));
        value = *base_ptr++;

        if(value != 0) {
            rr = (g_armwave_state.ch1_color.r * value) >> 8;
            gg = (g_armwave_state.ch1_color.g * value) >> 8;
            bb = (g_armwave_state.ch1_color.b * value) >> 8;

            r = MIN(rr, 255);
            g = MIN(gg, 255);
            b = MIN(bb, 255);

            // ensure 100% alpha channel, if it is used
            word = 0xff000000 | (b << 16) | (g << 8) | r;
        } else {
        	word = 0x00000000;
        }

        *out_buffer++ = word;
    }
}

void armwave_dump_ppm_debug(uint32_t *buffer, char *fn)
{
    FILE *fp = fopen(fn, "wb");
    uint32_t data;
    int xx, yy;

    //printf("in_buffer=0x%08x\n", buffer);

    fputs("P3\n", fp);
    fprintf(fp, "%d %d\n", g_armwave_state.target_height, g_armwave_state.target_width);
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

void armwave_test_init(float mod)
{
	mod_depth = mod;

    test_create_waveform();
    test_create_gamma();

    armwave_setup_render(&test_wave_buffer, 0, TEST_WAVE_SIZE, TEST_NWAVES, TEST_WAVE_SIZE, 2048, 256, 0x00000000);

    printf("armwave version: %s\n", ARMWAVE_VER);
}

void armwave_test_generate()
{
	uint32_t yy;

    memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.size);

    for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
        render_nonaa_to_buffer_1ch_slice(yy * g_armwave_state.slice_height, g_armwave_state.slice_record_height);
    }
}

PyObject *armwave_test_get_buffer()
{
    PyObject *mv;
    Py_buffer *buf = malloc(sizeof(Py_buffer));
    
    armwave_fill_pixbuf2(g_armwave_state.out_pixbuf);
    PyBuffer_FillInfo(buf, NULL, g_armwave_state.out_pixbuf, sizeof(uint32_t) * g_armwave_state.size, true, PyBUF_ND);

	mv = PyMemoryView_FromBuffer(buf);
}

void armwave_test_dump_buffer_to_ppm(char *fn)
{
	armwave_dump_ppm_debug(g_armwave_state.out_pixbuf, fn);
}

void armwave_cleanup()
{
	free(g_armwave_state.out_pixbuf);
	free(g_armwave_state.ch1_buffer);
	free(g_armwave_state.xcoord_to_xpixel);

	g_armwave_state.out_pixbuf = NULL;
	g_armwave_state.ch1_buffer = NULL;
	g_armwave_state.xcoord_to_xpixel = NULL;
}

int main(int argc, char *argv[])
{
    uint32_t *out_buffer;
    uint32_t xx, yy, n;

    //printf("Starting armwave...\n");
    armwave_init();

    //printf("Creating test waveform...\n");
    test_create_waveform();

    //printf("Creating gamma LUT...\n");
    test_create_gamma();

    //printf("Setting up render...\n");
    armwave_setup_render(&test_wave_buffer, 0, TEST_WAVE_SIZE, TEST_NWAVES, TEST_WAVE_SIZE, 2048, 256, 0x00000000);

    //printf("Wave buffer = 0x%08x (const ptr:0x%08x)\n", g_armwave_state.wave_buffer, &test_wave_buffer);

    for(n = 0; n < 5000; n++) {
        for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
            //printf("Rendering slice y=%d at y_pos=%d\n", yy, yy * g_armwave_state.slice_height);
            render_nonaa_to_buffer_1ch_slice(yy * g_armwave_state.slice_height, g_armwave_state.slice_record_height);
        }
    }

    /*
    printf("Creating pixbuf\n");
    out_buffer = malloc(sizeof(uint32_t) * g_armwave_state.size);
    armwave_fill_pixbuf(out_buffer);

    printf("Dumping pixbuf\n");
    armwave_dump_ppm_debug(out_buffer, "test.ppm");
    */

    return 0;
}