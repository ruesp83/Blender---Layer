Image Layer for Blender
=======================

I started to develop features for images to create layers [1][1] in Blender, as in PS and Gimp!!
A first implementation was done by KWK (Konrad Kleine) [2][2] for Gsoc2010.

I have drawn up a roadmap and started raising funds for the development of the Image Layer.
The project is divided for a total of 6 phases, where each stage has its budget.
Each phase addresses a particular aspect, such as: layer management, operations, etc..

Phases
------

### Phase 1:
    **Stage**    : Management
    Target   : Management and operations common to organize Layer.
    Deadline : 15/04/2012
    Amount   : 500€
    Info     : 
        1) Operations management layer.
            Menu Layer
                |-> Add
                    |-> New Layer
                    |-> ------------------
                    |-> Above active layer
                    |-> Below active layer
                |-> Duplicate Layer
                |-> Clear Layer
                |-> Remove Layer
                    |-> Layer
                    |-> Hidden Layers
                |-> Merge
                    |-> Merge Layers
                    |-> Merge Visible
                    |-> Unique layers
                |-> ------------------
                |-> Select
                    |-> Select the previous layer
                    |-> Select the next layer
                    |-> Select the top layer
                    |-> Select the bottom layer
                |-> Order
                    |-> Layer to Top
                    |-> Raise Layer
                    |-> Lower Layer
                    |-> Layer to Bottom
                    |-> ------------------
                    |-> Reverse Layer Order
        2) Insert image as a layer.
        3) Undo & Redo Layer.
        4) Lock Layer.
        5) Reading and saving in files. Blend.
        6) Dashed Border Layer.
        7) Improving and adding some other Blend Mode.


### Phase 2:
    Stage    : Transform
    Target   : Simple operation to change the selected layer.
    Deadline : Defined after phase 1
    Amount   : 800€
    Info     : 
        1) Operations for manipulating layers.
            Menu Layer
                |-> Order ...
                |-> Transform
                    |-> Flip Horizontally
                    |-> Flip Vertically
                    |-> ------------------
                    |-> Rotate 90° clockwise
                    |-> Rotate 90° counter-clockwise
                    |-> Rotate 180°
                    |-> Arbitrary Rotation
                    |-> ------------------
                    |-> Offset
                |-> Scale
                    |-> Layer Boundary Size
                    |-> Layer to Image Size
                    |-> Scale Layer
        2) Begin integration layer with Blender.
		3) Saving an image in a single layer (Save, Save As ..).


### Phase 3:
    Stage    : Color
    Target   : Operation for the color management.
    Deadline : Defined after phase 2
    Amount   : 1200€
    Info     : 
        1) Exposure, Saturation, Hue, Contrast, Color Temperature and Tint, Sharpness.
        2) Add GrayScale for the Color Space.
        3) Integration layer with Blender.


### Phase 4:
    Stage    : Import & Export
    Target   : Import and export in the formats most commonly used layer.
    Deadline : Defined after phase 3
    Amount   : 2000€
    Info     : Import and Export in ora, xcf, ...


### Phase 5: 
    Stage    : Tools
    Target   : Paint Tools, Selection Tools, Generic Tools
    Deadline : Defined after phase 4
    Amount   : 2500€
    Info     : 
        1) Paint Tools: Color Picker, Text Tool, Bucket Fill, Shapes.
        2) Selection Tools: Rectangle, Ellipse, Free, Polygonal.
        3) Generic Tools: Griglia, Hand Tool, ...


### Phase 6: 
    Stage    : Brush
    Target   : Improvement Brush 
    Deadline : Defined after phase 5
    Amount   : 3000€
    Info     :
        1) Import the improvements made by Jason Wilkins in GSOC 2011.
        2) Add Blend Mode for Brushes.
        3) To be defined yet!


Contact
-------
Fabio Russo (ruesp83)

- http://ruesp83.wordpress.com/
- https://github.com/ruesp83
- https://twitter.com/ruesp83
- https://www.facebook.com/ruesp83
- ruesp83@libero.it

[1]: http://en.wikipedia.org/wiki/Layers_%28digital_image_editing%29
[2]: http://wiki.blender.org/index.php?title=User:Kwk/Gsoc2010/ImageLayers