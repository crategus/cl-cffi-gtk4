;;; ----------------------------------------------------------------------------
;;; gsk.gl-shader.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GskGLShader
;;;
;;;     Fragment shaders for GSK
;;;
;;; Types and Values
;;;
;;;     GskGLShader
;;;     GskGLUniformType
;;;     GskShaderArgsBuilder
;;;
;;; Functions
;;;
;;;     gsk_gl_shader_new_from_bytes
;;;     gsk_gl_shader_new_from_resource
;;;     gsk_gl_shader_compile
;;;     gsk_gl_shader_get_source
;;;     gsk_gl_shader_get_resource
;;;     gsk_gl_shader_get_n_textures
;;;     gsk_gl_shader_get_n_uniforms
;;;     gsk_gl_shader_get_uniform_name
;;;     gsk_gl_shader_find_uniform_by_name
;;;     gsk_gl_shader_get_uniform_type
;;;     gsk_gl_shader_get_uniform_offset
;;;     gsk_gl_shader_get_args_size
;;;     gsk_gl_shader_get_arg_float
;;;     gsk_gl_shader_get_arg_int
;;;     gsk_gl_shader_get_arg_uint
;;;     gsk_gl_shader_get_arg_bool
;;;     gsk_gl_shader_get_arg_vec2
;;;     gsk_gl_shader_get_arg_vec3
;;;     gsk_gl_shader_get_arg_vec4
;;;     gsk_gl_shader_format_args_va
;;;     gsk_gl_shader_format_args
;;;
;;;     gsk_shader_args_builder_new
;;;     gsk_shader_args_builder_to_args
;;;     gsk_shader_args_builder_free_to_args
;;;     gsk_shader_args_builder_unref
;;;     gsk_shader_args_builder_ref
;;;     gsk_shader_args_builder_set_float
;;;     gsk_shader_args_builder_set_int
;;;     gsk_shader_args_builder_set_uint
;;;     gsk_shader_args_builder_set_bool
;;;     gsk_shader_args_builder_set_vec2
;;;     gsk_shader_args_builder_set_vec3
;;;     gsk_shader_args_builder_set_vec4
;;;
;;; Properties
;;;
;;;     resource
;;;     source
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GskGLShader
;;;
;;; Description
;;;
;;;A GskGLShader is a snippet of GLSL that is meant to run in the fragment shader of the rendering pipeline. A fragment shader gets the coordinates being rendered as input and produces the pixel values for that particular pixel. Additionally, the shader can declare a set of other input arguments, called uniforms (as they are uniform over all the calls to your shader in each instance of use). A shader can also receive up to 4 textures that it can use as input when producing the pixel data.

;;;GskGLShader is usually used with gtk_snapshot_push_gl_shader() to produce a GskGLShaderNode in the rendering hierarchy, and then its input textures are constructed by rendering the child nodes to textures before rendering the shader node itself. (You can pass texture nodes as children if you want to directly use a texture as input).

;;;The actual shader code is GLSL code that gets combined with some other code into the fragment shader. Since the exact capabilities of the GPU driver differs between different OpenGL drivers and hardware, GTK adds some defines that you can use to ensure your GLSL code runs on as many drivers as it can.

;;;If the OpenGL driver is GLES, then the shader language version is set to 100, and GSK_GLES will be defined in the shader.

;;;Otherwise, if the OpenGL driver does not support the 3.2 core profile, then the shader will run with language version 110 for GL2 and 130 for GL3, and GSK_LEGACY will be defined in the shader.

;;;If the OpenGL driver supports the 3.2 code profile, it will be used, the shader language version is set to 150, and GSK_GL3 will be defined in the shader.

;;;The main function the shader must implement is:

;;;void mainImage(out vec4 fragColor,
;;;               in vec2 fragCoord,
;;;               in vec2 resolution,
;;;               in vec2 uv)
;;;Where the input fragCoord is the coordinate of the pixel we're currently rendering, relative to the boundary rectangle that was specified in the GskGLShaderNode, and resolution is the width and height of that rectangle. This is in the typical GTK coordinate system with the origin in the top left. uv contains the u and v coordinates that can be used to index a texture at the corresponding point. These coordinates are in the [0..1]x[0..1] region, with 0, 0 being in the lower left corder (which is typical for OpenGL).

;;;The output fragColor should be a RGBA color (with premultiplied alpha) that will be used as the output for the specified pixel location. Note that this output will be automatically clipped to the clip region of the glshader node.

;;;In addition to the function arguments the shader can define up to 4 uniforms for textures which must be called u_textureN (i.e. u_texture1 to u_texture4) as well as any custom uniforms you want of types int, uint, bool, float, vec2, vec3 or vec4.

;;;All textures sources contain premultiplied alpha colors, but if some there are outer sources of colors there is a gsk_premultiply() helper to compute premultiplication when needed.

;;;Note that GTK parses the uniform declarations, so each uniform has to be on a line by itself with no other code, like so:

;;;uniform float u_time;
;;;uniform vec3 u_color;
;;;uniform sampler2D u_texture1;
;;;uniform sampler2D u_texture2;
;;;GTK uses the the "gsk" namespace in the symbols it uses in the shader, so your code should not use any symbols with the prefix gsk or GSK. There are some helper functions declared that you can use:

;;;vec4 GskTexture(sampler2D sampler, vec2 texCoords);
;;;This samples a texture (e.g. u_texture1) at the specified coordinates, and containes some helper ifdefs to ensure that it works on all OpenGL versions.

;;;You can compile the shader yourself using gsk_gl_shader_compile(), otherwise the GSK renderer will do it when it handling the glshader node. If errors occurs, the returned error will include the glsl sources, so you can see what GSK was passing to the compiler. You can also set GSK_DEBUG=shaders in the environment to see the sources and other relevant information about all shaders that GSK is handling.

;;;An example shader
;;;uniform float position;
;;;uniform sampler2D u_texture1;
;;;uniform sampler2D u_texture2;

;;;void mainImage(out vec4 fragColor,
;;;               in vec2 fragCoord,
;;;               in vec2 resolution,
;;;               in vec2 uv) {
;;;  vec4 source1 = GskTexture(u_texture1, uv);
;;;  vec4 source2 = GskTexture(u_texture2, uv);

;;;  fragColor = position * source1 + (1.0 - position) * source2;
;;;}
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; enum GskGLUniformType
;;;
;;; This defines the types of the uniforms that GskGLShaders declare. It defines 
;;; both what the type is called in the GLSL shader code, and what the 
;;; corresponding C type is on the Gtk side.
;;;
;;; GSK_GL_UNIFORM_TYPE_NONE
;;;     No type, used for uninitialized or unspecified values.
;;;
;;; GSK_GL_UNIFORM_TYPE_FLOAT
;;;     A float uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_INT
;;;     A GLSL int / gint32 uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_UINT
;;;     A GLSL uint / guint32 uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_BOOL
;;;     A GLSL bool / gboolean uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_VEC2
;;;     A GLSL vec2 / graphene_vec2_t uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_VEC3
;;;     A GLSL vec3 / graphene_vec3_t uniform
;;;
;;; GSK_GL_UNIFORM_TYPE_VEC4
;;;     A GLSL vec4 / graphene_vec4_t uniform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskShaderArgsBuilder
;;;
;;; typedef struct _GskShaderArgsBuilder GskShaderArgsBuilder;
;;; An object to build the uniforms data for a GskGLShader.
;;; ----------------------------------------------------------------------------

(cffi:defcstruct shader-args-builder)

(export 'shader-args-builder)

;;; ----------------------------------------------------------------------------
;;; GskGLShader
;;;
;;; typedef struct _GskGLShader GskGLShader;
;;; An object representing a GL shader program.
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GskGLShader" gl-shader
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gsk_gl_shader_get_type")
  ((resource
    gl-shader-resource
    "resource" "gchararray" t t)
   (source
    gl-shader-source
    "source" "GBytes" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;The “resource” property
;;;  “resource”                 char *
;;;Resource containing the source code for the shader.

;;;If the shader source is not coming from a resource, this will be NULL.

;;;Owner: GskGLShader

;;;Flags: Read / Write / Construct Only

;;;Default value: NULL

;;;The “source” property
;;;  “source”                   GBytes *
;;;The sourcecode for the shader.

;;;Owner: GskGLShader

;;;Flags: Read / Write / Construct Only


;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_new_from_bytes ()
;;;
;;; GskGLShader *
;;; gsk_gl_shader_new_from_bytes (GBytes *sourcecode);
;;;
;;; Creates a GskGLShader that will render pixels using the specified code.
;;;
;;; sourcecode :
;;;     GLSL sourcecode for the shader, as a GBytes
;;;
;;; Returns :
;;;     A new GskGLShader.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gsk_gl_shader_new_from_resource ()
;;;GskGLShader *
;;;gsk_gl_shader_new_from_resource (const char *resource_path);
;;;Creates a GskGLShader that will render pixels using the specified code.

;;;Parameters
;;;resource_path

;;;path to a resource that contains the GLSL sourcecode for the shader

;;;
;;;Returns
;;;A new GskGLShader.

;;;[transfer full]

;;;gsk_gl_shader_compile ()
;;;gboolean
;;;gsk_gl_shader_compile (GskGLShader *shader,
;;;                       GskRenderer *renderer,
;;;                       GError **error);
;;;Tries to compile the shader for the given renderer , and reports FALSE with an error if there is a problem. You should use this function before relying on the shader for rendering and use a fallback with a simpler shader or without shaders if it fails.

;;;Note that this will modify the rendering state (for example change the current GL context) and requires the renderer to be set up. This means that the widget has to be realized. Commonly you want to call this from the realize signal of a widget, or during widget snapshot.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;renderer

;;;a GskRenderer

;;;
;;;error

;;;location to store error in

;;;
;;;Returns
;;;TRUE on success, FALSE if an error occurred

;;;gsk_gl_shader_get_source ()
;;;GBytes *
;;;gsk_gl_shader_get_source (GskGLShader *shader);
;;;Gets the GLSL sourcecode being used to render this shader.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;Returns
;;;The source code for the shader.

;;;[transfer none]

;;;gsk_gl_shader_get_resource ()
;;;const char *
;;;gsk_gl_shader_get_resource (GskGLShader *shader);
;;;Gets the resource path for the GLSL sourcecode being used to render this shader.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;Returns
;;;The resource path for the shader, or NULL if none.

;;;[transfer none]

;;;gsk_gl_shader_get_n_textures ()
;;;int
;;;gsk_gl_shader_get_n_textures (GskGLShader *shader);
;;;Returns the number of textures that the shader requires.

;;;This can be used to check that the a passed shader works in your usecase. It is determined by looking at the highest u_textureN value that the shader defines.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;Returns
;;;The number of texture inputs required by shader

;;;gsk_gl_shader_get_n_uniforms ()
;;;int
;;;gsk_gl_shader_get_n_uniforms (GskGLShader *shader);
;;;Get the number of declared uniforms for this shader.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;Returns
;;;The number of declared uniforms

;;;gsk_gl_shader_get_uniform_name ()
;;;const char *
;;;gsk_gl_shader_get_uniform_name (GskGLShader *shader,
;;;                                int idx);
;;;Get the name of the declared uniform for this shader at index idx .

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The name of the declared uniform.

;;;[transfer none]

;;;gsk_gl_shader_find_uniform_by_name ()
;;;int
;;;gsk_gl_shader_find_uniform_by_name (GskGLShader *shader,
;;;                                    const char *name);
;;;Looks for a uniform by the name name , and returns the index of the uniform, or -1 if it was not found.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;name

;;;uniform name

;;;
;;;Returns
;;;The index of the uniform, or -1

;;;gsk_gl_shader_get_uniform_type ()
;;;GskGLUniformType
;;;gsk_gl_shader_get_uniform_type (GskGLShader *shader,
;;;                                int idx);
;;;Get the type of the declared uniform for this shader at index idx .

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The type of the declared uniform

;;;gsk_gl_shader_get_uniform_offset ()
;;;int
;;;gsk_gl_shader_get_uniform_offset (GskGLShader *shader,
;;;                                  int idx);
;;;Get the offset into the data block where data for this uniforms is stored.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The data offset

;;;gsk_gl_shader_get_args_size ()
;;;gsize
;;;gsk_gl_shader_get_args_size (GskGLShader *shader);
;;;Get the size of the data block used to specify arguments for this shader.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;Returns
;;;The size of the data block

;;;gsk_gl_shader_get_arg_float ()
;;;float
;;;gsk_gl_shader_get_arg_float (GskGLShader *shader,
;;;                             GBytes *args,
;;;                             int idx);
;;;Gets the value of the uniform idx in the args block. The uniform must be of float type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The value

;;;gsk_gl_shader_get_arg_int ()
;;;gint32
;;;gsk_gl_shader_get_arg_int (GskGLShader *shader,
;;;                           GBytes *args,
;;;                           int idx);
;;;Gets the value of the uniform idx in the args block. The uniform must be of int type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The value

;;;gsk_gl_shader_get_arg_uint ()
;;;guint32
;;;gsk_gl_shader_get_arg_uint (GskGLShader *shader,
;;;                            GBytes *args,
;;;                            int idx);
;;;Gets the value of the uniform idx in the args block. The uniform must be of uint type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The value

;;;gsk_gl_shader_get_arg_bool ()
;;;gboolean
;;;gsk_gl_shader_get_arg_bool (GskGLShader *shader,
;;;                            GBytes *args,
;;;                            int idx);
;;;Gets the value of the uniform idx in the args block. The uniform must be of bool type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;Returns
;;;The value

;;;gsk_gl_shader_get_arg_vec2 ()
;;;void
;;;gsk_gl_shader_get_arg_vec2 (GskGLShader *shader,
;;;                            GBytes *args,
;;;                            int idx,
;;;                            graphene_vec2_t *out_value);
;;;Gets the value of the uniform idx in the args block. The uniform must be of vec2 type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;out_value

;;;location to store the uniform value in

;;;
;;;gsk_gl_shader_get_arg_vec3 ()
;;;void
;;;gsk_gl_shader_get_arg_vec3 (GskGLShader *shader,
;;;                            GBytes *args,
;;;                            int idx,
;;;                            graphene_vec3_t *out_value);
;;;Gets the value of the uniform idx in the args block. The uniform must be of vec3 type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;out_value

;;;location to store the uniform value in

;;;
;;;gsk_gl_shader_get_arg_vec4 ()
;;;void
;;;gsk_gl_shader_get_arg_vec4 (GskGLShader *shader,
;;;                            GBytes *args,
;;;                            int idx,
;;;                            graphene_vec4_t *out_value);
;;;Gets the value of the uniform idx in the args block. The uniform must be of vec4 type.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;args

;;;uniform arguments

;;;
;;;idx

;;;index of the uniform

;;;
;;;out_value

;;;location to store set the uniform value in

;;;
;;;gsk_gl_shader_format_args_va ()
;;;GBytes *
;;;gsk_gl_shader_format_args_va (GskGLShader *shader,
;;;                              va_list uniforms);
;;;Formats the uniform data as needed for feeding the named uniforms values into the shader. The argument list is a list of pairs of names, and values for the types that match the declared uniforms (i.e. double/int/guint/gboolean for primitive values and graphene_vecN_t * for vecN uniforms).

;;;It is an error to pass a uniform name that is not declared by the shader.

;;;Any uniforms of the shader that are not included in the argument list are zero-initialized.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;uniforms

;;;name-Value pairs for the uniforms of shader , ending with a NULL name

;;;
;;;Returns
;;;A newly allocated block of data which can be passed to gsk_gl_shader_node_new().

;;;[transfer full]

;;;gsk_gl_shader_format_args ()
;;;GBytes *
;;;gsk_gl_shader_format_args (GskGLShader *shader,
;;;                           ...);
;;;Formats the uniform data as needed for feeding the named uniforms values into the shader. The argument list is a list of pairs of names, and values for the types that match the declared uniforms (i.e. double/int/guint/gboolean for primitive values and graphene_vecN_t * for vecN uniforms).

;;;Any uniforms of the shader that are not included in the argument list are zero-initialized.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;...

;;;name-Value pairs for the uniforms of shader , ending with a NULL name

;;;
;;;Returns
;;;A newly allocated block of data which can be passed to gsk_gl_shader_node_new().

;;;[transfer full]

;;;gsk_shader_args_builder_new ()
;;;GskShaderArgsBuilder *
;;;gsk_shader_args_builder_new (GskGLShader *shader,
;;;                             GBytes *initial_values);
;;;Allocates a builder that can be used to construct a new uniform data chunk.

;;;Parameters
;;;shader

;;;a GskGLShader

;;;
;;;initial_values

;;;optional Bytes with initial values.

;;;[nullable]
;;;Returns
;;;The newly allocated builder, free with gsk_shader_args_builder_unref().

;;;[transfer full]

;;;gsk_shader_args_builder_to_args ()
;;;GBytes *
;;;gsk_shader_args_builder_to_args (GskShaderArgsBuilder *builder);
;;;Creates a new GBytes args from the current state of the given builder . Any uniforms of the shader that have not been explicitly set on the builder are zero-initialized.

;;;The given GskShaderArgsBuilder is reset once this function returns; you cannot call this function multiple times on the same builder instance.

;;;This function is intended primarily for bindings. C code should use gsk_shader_args_builder_free_to_args().

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;Returns
;;;the newly allocated builder, free with gsk_shader_args_builder_free().

;;;[transfer full]

;;;gsk_shader_args_builder_free_to_args ()
;;;GBytes *
;;;gsk_shader_args_builder_free_to_args (GskShaderArgsBuilder *builder);
;;;Creates a new GBytes args from the current state of the given builder , and frees the builder instance. Any uniforms of the shader that have not been explicitly set on the builder are zero-initialized.

;;;[skip]

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;Returns
;;;the newly created GBytes with all the args added to builder .

;;;[transfer full]

;;;gsk_shader_args_builder_unref ()
;;;void
;;;gsk_shader_args_builder_unref (GskShaderArgsBuilder *builder);
;;;Decreases the reference count of a GskShaderArgBuilder by one. If the resulting reference count is zero, frees the builder.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;gsk_shader_args_builder_ref ()
;;;GskShaderArgsBuilder *
;;;gsk_shader_args_builder_ref (GskShaderArgsBuilder *builder);
;;;Increases the reference count of a GskShaderArgsBuilder by one.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;Returns
;;;the passed in GskShaderArgsBuilder

;;;gsk_shader_args_builder_set_float ()
;;;void
;;;gsk_shader_args_builder_set_float (GskShaderArgsBuilder *builder,
;;;                                   int idx,
;;;                                   float value);
;;;Sets the value of the uniform idx . The uniform must be of float type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform to

;;;
;;;gsk_shader_args_builder_set_int ()
;;;void
;;;gsk_shader_args_builder_set_int (GskShaderArgsBuilder *builder,
;;;                                 int idx,
;;;                                 gint32 value);
;;;Sets the value of the uniform idx . The uniform must be of int type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform to

;;;
;;;gsk_shader_args_builder_set_uint ()
;;;void
;;;gsk_shader_args_builder_set_uint (GskShaderArgsBuilder *builder,
;;;                                  int idx,
;;;                                  guint32 value);
;;;Sets the value of the uniform idx . The uniform must be of uint type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform to

;;;
;;;gsk_shader_args_builder_set_bool ()
;;;void
;;;gsk_shader_args_builder_set_bool (GskShaderArgsBuilder *builder,
;;;                                  int idx,
;;;                                  gboolean value);
;;;Sets the value of the uniform idx . The uniform must be of bool type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform to

;;;
;;;gsk_shader_args_builder_set_vec2 ()
;;;void
;;;gsk_shader_args_builder_set_vec2 (GskShaderArgsBuilder *builder,
;;;                                  int idx,
;;;                                  const graphene_vec2_t *value);
;;;Sets the value of the uniform idx . The uniform must be of vec2 type.

;;;Parameters
;;;builder

;;;A GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform too

;;;
;;;gsk_shader_args_builder_set_vec3 ()
;;;void
;;;gsk_shader_args_builder_set_vec3 (GskShaderArgsBuilder *builder,
;;;                                  int idx,
;;;                                  const graphene_vec3_t *value);
;;;Sets the value of the uniform idx . The uniform must be of vec3 type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform too

;;;
;;;gsk_shader_args_builder_set_vec4 ()
;;;void
;;;gsk_shader_args_builder_set_vec4 (GskShaderArgsBuilder *builder,
;;;                                  int idx,
;;;                                  const graphene_vec4_t *value);
;;;Sets the value of the uniform idx . The uniform must be of vec4 type.

;;;Parameters
;;;builder

;;;a GskShaderArgsBuilder

;;;
;;;idx

;;;index of the uniform

;;;
;;;value

;;;value to set the uniform too

;;; --- End of file gsk.gl-shader.lisp -----------------------------------------
