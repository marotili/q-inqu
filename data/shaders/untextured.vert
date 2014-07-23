#version 330

out vec4 color_pass;
uniform mat4 view;
uniform mat4 projection;

in vec2 pos;
in vec4 myColor;


void main()
{
    gl_Position = projection*view*vec4(pos, 0, 1);
    color_pass = myColor;
}
