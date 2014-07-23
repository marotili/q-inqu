#version 330
layout(location = 0) out vec4 color_final;

in vec4 color_pass;

void main()
{
    color_final = color_pass;
}
