R"(#version 450
precision highp float;

struct Material {
    vec3 ka, kd, ks;
    float  shininess;
    vec3 F0;
    int rough, reflective;
};

struct Light {
    vec3 direction;
    vec3 Le, La;
};

struct Plane {
    vec3 point;
    vec3 normal;
    int mat;
};

struct Ellipsoid {
    vec3 center;
    vec3 params;
    vec3 color;
    int mat;
};

struct Hit {
    float t;
    vec3 position, normal;
    int mat;	// material index
};

struct Ray {
    vec3 start, dir;
};

const int nMaxEllipsoid = 10;
const int nMaxMirror = 100;

uniform vec3 wEye;
uniform Light light;
uniform Material materials[2]; // TODO

uniform Plane bottom;

uniform int mirrorMaterial;
uniform int nMirror;
uniform Plane mirrors[nMaxMirror];

uniform int nEllipsoid;
uniform Ellipsoid ellipsoid[nMaxEllipsoid];

in  vec3 p;					// point on camera window corresponding to the pixel
out vec4 fragmentColor;		// output that goes to the raster memory as told by glBindFragDataLocation

Hit intersect(const Plane object, const Ray ray) {
    Hit hit;
    hit.t = -1;
    float D = dot(object.normal, object.point);
    float counter = D - dot(object.normal, ray.start);
    float div = dot(object.normal, ray.dir);
    if (div == 0) return hit;
    hit.t = counter / div;
    hit.normal = object.normal;
    hit.mat = object.mat;
    hit.position = ray.start + ray.dir * hit.t;
    return hit;
}

Hit intersect(const Ellipsoid object, const Ray ray) {
    Hit hit;
    hit.t = -1;
    mat3 M = mat3(
        1/object.params.x, 0, 0,
        0, 1/object.params.y, 0,
        0, 0, 1/object.params.z
    );
    vec3 v1 = ray.dir * M;
    vec3 P1 = ray.start * M - object.center * M;
    float a = dot(v1, v1);
    float b = 2.0 * dot(P1, v1);
    float c = dot(P1, P1) - 1.0;
    float discr = b *b - 4.0 * a * c;
    if (discr < 0) return hit;
    float sqrt_discr = sqrt(discr);
    float t1 = (-b + sqrt_discr) / 2.0 / a;
    float t2 = (-b - sqrt_discr) / 2.0 / a;
    if (t1 <= 0) return hit;
    hit.t = (t2 > 0) ? t2 : t1;
    hit.position = ray.start + ray.dir * hit.t;
    vec3 helper = hit.t - object.center;
    hit.normal = 2.0 * vec3(
        helper.x / object.params.x / object.params.x,
        helper.y / object.params.y / object.params.y,
        helper.z / object.params.z / object.params.z
    );
    hit.mat = object.mat;
    return hit;
}

Hit firstIntersect(Ray ray) {
    Hit bestHit;
    bestHit.t = -1;
    for (int o = 0; o < nMirror; o++) {
        Hit hit = intersect(mirrors[o], ray); //  hit.t < 0 if no intersection
        if (hit.t > 0 && (bestHit.t < 0 || hit.t < bestHit.t))  bestHit = hit;
    }
    Hit hit = intersect(bottom, ray); //  hit.t < 0 if no intersection
    if (hit.t > 0 && (bestHit.t < 0 || hit.t < bestHit.t))  bestHit = hit;

    for (int o = 0; o < nEllipsoid; o++) {
        Hit hit = intersect(ellipsoid[o], ray); //  hit.t < 0 if no intersection
        if (hit.t > 0 && (bestHit.t < 0 || hit.t < bestHit.t))  bestHit = hit;
    }
    if (dot(ray.dir, bestHit.normal) > 0) bestHit.normal = bestHit.normal * (-1);
    return bestHit;
}

bool shadowIntersect(Ray ray) {	// for directional lights
    for (int o = 0; o < nEllipsoid; o++) if (intersect(ellipsoid[o], ray).t > 0) return true; //  hit.t < 0 if no intersection
    return false;
}

vec3 Fresnel(vec3 F0, float cosTheta) {
    return F0 + (vec3(1, 1, 1) - F0) * pow(cosTheta, 5);
}

const float epsilon = 0.0001f;
const int maxdepth = 5;

vec3 trace(Ray ray) {
    vec3 weight = vec3(1, 1, 1);
    vec3 outRadiance = vec3(0, 0, 0);
    for(int d = 0; d < maxdepth; d++) {
        Hit hit = firstIntersect(ray);
        if (hit.t < 0) return weight * light.La;
        if (materials[hit.mat].rough == 1) {
            outRadiance += weight * materials[hit.mat].ka * light.La;
            Ray shadowRay;
            shadowRay.start = hit.position + hit.normal * epsilon;
            shadowRay.dir = light.direction;
            float cosTheta = dot(hit.normal, light.direction);
            if (cosTheta > 0 && !shadowIntersect(shadowRay)) {
                outRadiance += weight * light.Le * materials[hit.mat].kd * cosTheta;
                vec3 halfway = normalize(-ray.dir + light.direction);
                float cosDelta = dot(hit.normal, halfway);
                if (cosDelta > 0) outRadiance += weight * light.Le * materials[hit.mat].ks * pow(cosDelta, materials[hit.mat].shininess);
            }
        }

        if (materials[hit.mat].reflective == 1) {
            weight *= Fresnel(materials[hit.mat].F0, dot(-ray.dir, hit.normal));
            ray.start = hit.position + hit.normal * epsilon;
            ray.dir = reflect(ray.dir, hit.normal);
        } else return outRadiance;
    }
}

void main() {
    Ray ray;
    ray.start = wEye;
    ray.dir = normalize(p - wEye);
    fragmentColor = vec4(trace(ray), 1);
}
)"