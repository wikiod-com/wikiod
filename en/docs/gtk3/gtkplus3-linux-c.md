---
title: "gtk+3 linux c"
slug: "gtk+3-linux-c"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

code samples and some other stuff

## css in action
    #include <gtk/gtk.h>//jjk.c
    static void destroy(GtkWidget *widget, gpointer data)
    {
    gtk_main_quit();
    }
    int main(int argc, char *argv[])
    {
    gtk_init(&argc, &argv);
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Window");
    g_signal_connect(window, "destroy", G_CALLBACK(destroy), NULL);
    
    GtkWidget *k;
    k= gtk_fixed_new();
        gtk_container_add(GTK_CONTAINER(window), k);
        
    
           GtkWidget* la,*r;
        la = gtk_button_new_with_label (",mkl");
         gtk_fixed_put (GTK_FIXED (k), la,50,237);
        gtk_widget_set_size_request(la, 98, 90);
    //    gtk_container_set_border_width(GTK_CONTAINER (la)    , 5);
    
    union {
    char w[4]={0xf,0xe,0xd,0xa};;
    uint t;
    } tc;
    
    
    
    GtkCssProvider *provider = gtk_css_provider_new ();
    gtk_css_provider_load_from_path (provider,
        "/home/alex/gtk-widgets.css", NULL);
    
        r = gtk_button_new_with_label (",kii");
         gtk_fixed_put (GTK_FIXED (k), r,150,237);
        gtk_widget_set_size_request(r, 98, 90);
    
        gtk_widget_set_size_request(GTK_WIDGET(window),300,349);
        
    GtkStyleContext *context;
            context = gtk_widget_get_style_context(la);
    gtk_style_context_add_provider (context,
                                        GTK_STYLE_PROVIDER(provider),
                                        GTK_STYLE_PROVIDER_PRIORITY_USER);
    
    //    gtk_style_context_save (context);
    //    gtk_style_context_add_provider_for_screen(gdk_screen_get_default(),
    //                                              GTK_STYLE_PROVIDER(provider),TK_STYLE_PROVIDER_PRIORITY_USER);
    
    
    gtk_widget_show_all(GTK_WIDGET(window));
    #define h 7
    
    
    printf("%xh\n",tc.t);
    gtk_main();
    return 0;
    }


gtk-widgets.css




    GtkButton:hover {
         color: yellowgreen;
        background-color: green;
        opacity: 0.95;
        text-decoration: underline;
        background-image:  -gtk-gradient (linear,
                 left top,
                 left bottom,
                 color-stop(0.0,rgba(34,97,70,1)),
                 color-stop(0.30,rgba(56,145,118,0.9)),
                 color-stop(0.81,rgba(34,131,116,0.9)),
                 color-stop(1.00,rgba(104,191,134,1)));
      
                 
    }
    
    GtkButton:active{
         color: yellowgreen;
        background-color: green;
        opacity: 0.97;
        text-decoration: underline;
        background-image:  -gtk-gradient (linear,
                 left top,
                 left bottom,
                 color-stop(0.000,rgba(104,191,134,1)),
                 color-stop(0.11,rgba(34,131,116,1)),
                 color-stop(0.32,rgba(34,97,70,1)),
                 color-stop(0.70,rgba(56,145,118,1)),
                 color-stop(0.91,rgba(34,131,116,1)),
                 color-stop(1.00,rgba(104,191,134,1)));
      
                 
    }
    
    
    
    GtkButton {
         color: yellowgreen;
        background-color: green;
        opacity: 0.797;
        text-decoration: underline;
        background-image:  -gtk-gradient (linear,
                 left top,
                 left bottom,
                 color-stop(0.0,rgba(34,97,70,1)),
                 color-stop(0.50,rgba(56,145,118,1)),
                 color-stop(0.51,rgba(34,131,116,1)),
                 color-stop(1.00,rgba(104,191,134,1)));
      
                 
    }

       
 

    c++ jjk.c --target=arm-linux-gnu  `pkg-config --libs --cflags gtk+-3.0` -o op 




## glarea sample

Shaders 3.3 + extension.seen at radeon hd5500 

    #version 330

triangles deployed so that normal to point out of a cube.trace each triangle 3points order.

    

    //#include <stdio.h>
    //#include <string.h>
    //#include <iostream>
    
    //#include <glib.h>
    
    #include <gdk/gdkx.h>
    #include <epoxy/glx.h>
    #include <epoxy/gl.h>
    #include <gtk/gtk.h>
    #include <glm/glm.hpp>
    #include <glm/gtc/matrix_transform.hpp>
    #include <glm/gtc/type_ptr.hpp>
    
    
    
    
    
    const GLchar *vert_src ="\n" \
    "#version 330                                  \n" \
    "          #extension GL_ARB_explicit_uniform_location : enable                                    \n" \
    "                                                   \n" \
    "                   out  vec4 knn;                                \n" \
    "           layout(location = 0) in vec4 in_position;                                       \n" \
    "           layout(location =10)uniform  mat4 projection;                                       \n" \
        "           out  vec4 P;                                       \n" \
    "void main()                                   \n" \
    "{                                             \n" \
    "  gl_Position =  projection*in_position;  \n" \
    "knn=in_position  ;  \n" \
    "                                                  \n" \
    "//  gl_Position =  in_position;  \n" \
    "}                                   \n" ;
    const GLchar *frag_src ="\n" \
    "#version 330                                  \n" \
            "             vec4 b;                                       \n" \
    "    in  vec4 knn;                                                \n" \
    "void main (void)                              \n" \
    "{                                             \n" \
    " if(gl_FrontFacing){                               \n" \
    
    "   b= vec4(gl_FragCoord.x/10.0,gl_FragCoord.y/357.0,gl_FragCoord.z*    0.5,1.0);                                                 \n" \
    "      gl_FragColor = vec4(0.021,-.1777*(knn.y+2)+0.1+1.21557*knn.x+knn.z  ,1.215*knn.z+0.583*knn.y +.0357*knn.x+3*b.y, 1.0);    \n" \
    "       }                                    \n" \
    "else  discard; \n" \
    "                                                   \n" \
    "                                                   \n" \
    "}                                            \n";
    
    
    
    
    GLfloat t[16]={ 1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1};
    glm::mat4 yt,b;
    
    
    GLuint gl_vao, gl_buffer, gl_program;
    
    static gboolean realise(GtkGLArea *area, GdkGLContext *context)
    {
    
    
      gtk_gl_area_make_current(GTK_GL_AREA(area));
      if (gtk_gl_area_get_error (GTK_GL_AREA(area)) != NULL)
      {
        printf("failed to initialiize buffers\n");
        return false;
      }
    
      GLfloat verts[] = 
      {   
         +0.7,+0.7,+0.7, 
             -0.7,0.7, 0.7,
             -0.7,+0.7, -0.7,
    
             0.7,+0.7, -0.7,
             +0.7,+0.7,+0.7,
             -0.7,+0.7, -0.7,
    
             -0.7,-0.7, +0.7,
             +0.7,+0.7,+0.7,
            -0.7,0.7, 0.7,
    
             +0.7,+0.7,+0.7,
             -0.7,-0.7, +0.7,
             0.7,-0.7, 0.7,
    
             +0.7,-0.7,-0.7,
             -0.7,-0.7,-0.7,
             0.7,+0.7, -0.7,
    
           -0.7,+0.7, -0.7,
        0.7,+0.7, -0.7,
           -0.7,-0.7, -0.7,
    
    
    
             -0.7,-0.7, +0.7,
        -0.7,-0.7, -0.7,
             0.7,-0.7, 0.7,
    
    
    
    +0.7,-0.7, -0.7,
    
    0.7,-0.7, 0.7,
    
    -0.7,-0.7, -0.7,
    
                      0.7,+0.7, -0.7,
             +0.7,+0.7,+0.7,
    +0.7,-0.7, -0.7,
    
             0.7,-0.7, -0.7,
                  +0.7,+0.7,+0.7,
     
     
      0.7,-0.7, 0.7,
    
    
                     -0.7,-0.7, -0.7,
            -0.7,0.7, 0.7,
     -0.7,+0.7, -0.7,
    
    
    
             -0.7,-0.7, +0.7,
            -0.7,0.7, 0.7,
             -0.7,-0.7, -0.7
    
      };
    b=glm::lookAt(glm::vec3(1.75,-2.38,1.4444), glm::vec3( 0., 0., 0.),glm::vec3( 0.,0.2,-00.));
    yt=glm::perspective(45., 1., 1.2, 300.);
    b=yt*b*glm::mat4(1.);
    
    //b=glm::lookAt(glm::vec3(0., 0.,-1.),glm::vec3( 0., 0., 0.),glm::vec3( 0.,025.,-1.));
    //yt=yt*b;
    
    
    
    
    
      GLuint frag_shader, vert_shader;
      frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
      vert_shader = glCreateShader(GL_VERTEX_SHADER);
    
    
      glShaderSource(frag_shader, 1, &frag_src, NULL);
      glShaderSource(vert_shader, 1, &vert_src, NULL);
    
      glCompileShader(frag_shader);
      glCompileShader(vert_shader);
    
      gl_program = glCreateProgram();
      glAttachShader(gl_program, frag_shader);
      glAttachShader(gl_program, vert_shader);
      glLinkProgram(gl_program);
    
    // glUniformMatrix4fv(1, 1, 0, glm::value_ptr(b));
    
      glGenVertexArrays(1, &gl_vao);
      glBindVertexArray(gl_vao);
    
    
    
      glGenBuffers(1, &gl_buffer);
      glBindBuffer(GL_ARRAY_BUFFER, gl_buffer);
      glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);
    
    
    
      glEnableVertexAttribArray(0);
      glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);
      glBindVertexArray(0);
      
    
      return TRUE;
    }
    
    static gboolean render(GtkGLArea *area, GdkGLContext *context)
    {
     
    
    
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
    
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glClearColor(0.0, 0.0, 0.0, 1.0);
    
    
    
      glUseProgram(gl_program);
     glUniformMatrix4fv(10, 1, 0, &b[0][0]);
    
    
      glBindVertexArray(gl_vao);
    
    
      glDrawArrays(GL_TRIANGLES,0,36 );
    
      glBindVertexArray (0);
      glUseProgram (0);
    
    
      glFlush();
    
      return TRUE;
    }
    
    int main(int argc, char** argv)
    {
      gtk_init(&argc, &argv);
    
      GtkWidget *window  = gtk_window_new(GTK_WINDOW_TOPLEVEL),
                *gl_area = gtk_gl_area_new();
    
      g_signal_connect(window,  "delete-event", G_CALLBACK(gtk_main_quit), NULL);
      g_signal_connect(gl_area, "realize",      G_CALLBACK(realise),       NULL);
      g_signal_connect(gl_area, "render",       G_CALLBACK(render),        NULL);
    
      gtk_container_add(GTK_CONTAINER(window), gl_area);
    
      gtk_widget_show_all(window);
    
      gtk_main();
    
      return 0;
    }



