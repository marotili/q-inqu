#version 330

out vec4 color;
uniform int numLights = 0;
in vec4 position;

struct Light {
	vec2 pos;
	float intensity;
	float fadeDistance;
};

layout(std140) uniform LightUBO {
	Light lights[5];
};


void main()
{
	bool visible = false;
	vec2 fragPos = position.xy;
	float alpha = 0;
	for (int i = 0; i < numLights; i++) {
		vec2 lightPos = -lights[i].pos;	
		float distToLight = length(lightPos - fragPos);
		if (distToLight <= lights[i].intensity) {
			visible = true;
			if (distToLight >= lights[i].fadeDistance) {
				alpha = (distToLight - lights[i].fadeDistance)/(lights[i].intensity - lights[i].fadeDistance);
			} 
		}
	}
	if (visible) {
		color = vec4(0, 0, 0, alpha);
	} else {
		color = vec4(0, 0, 0, 1);
	}
}