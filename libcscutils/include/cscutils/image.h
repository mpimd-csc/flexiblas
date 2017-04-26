/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2015 Martin Koehler
* 
* This library is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; either version 2.1 of the License, or
* (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this library; if not, see <http://www.gnu.org/licenses/>.
* 
*/ 

#ifndef CSC_IMAGE_H

#define CSC_IMAGE_H

#ifdef __cplusplus
extern "C" { 
#endif
	/**
	 @file libcscutils/include/cscutils/image.h
	  @defgroup image Image: Easy Interface for various image files. 

	  This part of the library contains some function to operate with images. At 
	  the moment only bmp files are supported. 
	  
	  \attention This part of the library depends on the \ref error_message module.
	  \attention The functions are only tested on little endian systems. 

	  @addtogroup image
	  @{ 
	*/
	
	/**
	 * @brief Representation of a bitmap file. 
	 *
	 * The csc_image_bmp structure contains functions to represent a bitmap file.
	 */
	typedef struct {
		unsigned char *red;	/**< Array representing colour red   */
		unsigned char *green; 	/**< Array representing colour green */
		unsigned char *blue;   	/**< Array representing colour blue */
		int bpp; 		/**< Bits per pixel */
		int width;		/**< Width of BMP file */
		int height;		/**< Height of BMP file */
	} csc_image_bmp;

	/**
	 * @brief Type definition of a colour for bitmap files. 
	 * 
	 * The csc_image_color type definition defines the color for bitmap files.
	 */
	typedef unsigned char csc_image_color;

	/**
	 * @brief Read data of a bitmap file.
	 * @param [in] filein_name name of input file
	 * @param[in] pic bitmap file
	 * @return zero on success or a non zero error value
	 *
	 * The csc_image_bmp_read function reads header information, palette information and image data of a bitmap file \f$ pic \f$.
	 *
	 */
	int csc_image_bmp_read       ( char *filein_name, csc_image_bmp *pic);

	/**
	 * @brief Write to a bitmap file.
	 * @param [in] fileout_name name of input file
	 * @param[in] pic bitmap file
	 * @return zero on success or a non zero error value
	 *
	 * The csc_image_bmp_write function writes header information and image data to bitmap file \f$ pic \f$.
	 *
	 */
	int csc_image_bmp_write      ( const char *fileout_name, csc_image_bmp pic);
	
	/**
	 * @brief Set pixel of a bitmap file.
	 * @param[in,out] pic bitmap file
	 * @param [in] x \f$ x \f$ coordinate of pixel
	 * @param [in] y \f$ y \f$ coordinate of pixel
	 * @param [in] r colour red
	 * @param [in] g colour green
	 * @param [in] b colour blue
	 * 
	 * The csc_image_bmp_set_pixel function sets the colour of a pixel in a bitmap file.
	 *
	 */
	void csc_image_bmp_set_pixel ( csc_image_bmp pic, int x, int y, csc_image_color r, csc_image_color g, csc_image_color b);
	
	/**
	 * @brief Fill a bitmap file with colour.
	 * @param[in,out] pic bitmap file
	 * @param [in] x \f$ x \f$ coordinate of pixel
	 * @param [in] y \f$ y \f$ coordinate of pixel
	 * @param [in] width width of bitmap file
	 * @param [in] height height of bitmap file
	 * @param [in] r colour red
	 * @param [in] g colour green
	 * @param [in] b colour blue
	 * 
	 * The csc_image_bmp_fill_rect function fills a bitmap file with colour. <br>
	 * This function is a wrapper around \ref bmp_set_pixel.
	 * 
	 * \sa csc_image bmp_set_pixel
	 *
	 */
	void csc_image_bmp_fill_rect ( csc_image_bmp pic, int x, int y, int width, int height, csc_image_color r, csc_image_color g, csc_image_color b);

	/**
	 * @brief Initialize a bitmap file.
	 * @param[in,out] pic bitmap file
	 * @param [in] width   width of bitmap file
	 * @param [in] height height of bitmap file
	 * @return zero on success or a non zero error value
	 *
	 * The csc_image_bmp_init function initializes a bitmap file \f$ pic \f$ with width \f$ width \f$ and height \f$ height \f$.
	 *
	 */
	int csc_image_bmp_init           ( csc_image_bmp *pic, int width, int height );
	
	/**
	 * @brief Clean up a bitmap file.
	 * @param[in,out] pic bitmap file
	 * 
	 * The csc_image_bmp_clear function cleans up a bitmap file.
	 * 
	 */
	void csc_image_bmp_clear     ( csc_image_bmp *pic);




	/** @}   */


#ifdef __cplusplus
}; 
#endif 

#endif /* end of include guard: CSC_IMAGE_H */

