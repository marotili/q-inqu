#version 330
layout(location = 0) out vec4 color_final;

in vec4 color_pass;
in vec2 texCoord;

uniform sampler2D Texture0;

void main()
{

    vec4 color = texture(Texture0, texCoord);
    color_final = vec4(color.r, color.r, color.r, color.r);
}
