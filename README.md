# C64 Spritemultiplexer

## Based on:
Spritemultiplexing example V2.1
by Lasse Öörni (loorni@gmail.com)
Available at http://cadaver.github.io

**Additions** by Ricki Sickenger (ricki.sickenger@gmail.com)


Quite easy (?) to understand example how to make a spritemultiplexer,
using 32 sprites. The routine is capable of more but the screen starts
to become very crowded, as they move randomly... 

Uses a "new" more optimal sortmethod that doesn't take as much time
as bubblesort. This method is based on the idea of an orderlist that
is not recreated from scratch each frame; but instead modified every
frame to create correct top-bottom order of sprites.                
                                                                    
Why sorted top-bottom order of sprites is necessary for multiplexing:
because raster interrupts are used to "rewrite" the sprite registers
in the middle of the screen and raster interrupts follow the        
top->bottom movement of the TV/monitor electron gun as it draws each
frame.                                                              
                                                                    
Light grey color in the bottom of the screen measures the time taken
by sprite sorting.                                                  
                                                                    
**What is missing from this tutorial for sake of simplicity:**
* Elimination of "extra" (more than 8) sprites on a row             
                                                                    

**Additions by Ricki:**

* Added support for multicolor sprites                              
* Added X-position msb table checks                                 
* Converted to ACME format                                          
 
**To use multicolor sprites:**
* Set the byte at the correct index in the 'sprmc'-table to a non-zero value.
 The 'sprmc' index follows the 'sprf' table index.

**To set the sprite msb x position:**
* Set the byte at the correct index in the 'sprx_msb'-table to
 a non-zero value.       

*This source code is in ACME format.*
