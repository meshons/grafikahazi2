//=============================================================================================
// Mintaprogram: Zold haromszog. Ervenyes 2018. osztol.
//
// A beadott program csak ebben a fajlban lehet, a fajl 1 byte-os ASCII karaktereket tartalmazhat, BOM kihuzando.
// Tilos:
// - mast "beincludolni", illetve mas konyvtarat hasznalni
// - faljmuveleteket vegezni a printf-et kiveve
// - Mashonnan atvett programresszleteket forrasmegjeloles nelkul felhasznalni es
// - felesleges programsorokat a beadott programban hagyni!!!!!!!
// - felesleges kommenteket a beadott programba irni a forrasmegjelolest kommentjeit kiveve
// ---------------------------------------------------------------------------------------------
// A feladatot ANSI C++ nyelvu forditoprogrammal ellenorizzuk, a Visual Studio-hoz kepesti elteresekrol
// es a leggyakoribb hibakrol (pl. ideiglenes objektumot nem lehet referencia tipusnak ertekul adni)
// a hazibeado portal ad egy osszefoglalot.
// ---------------------------------------------------------------------------------------------
// A feladatmegoldasokban csak olyan OpenGL fuggvenyek hasznalhatok, amelyek az oran a feladatkiadasig elhangzottak
// A keretben nem szereplo GLUT fuggvenyek tiltottak.
//
// NYILATKOZAT
// ---------------------------------------------------------------------------------------------
// Nev    : Stork Gabor
// Neptun : NO047V
// ---------------------------------------------------------------------------------------------
// ezennel kijelentem, hogy a feladatot magam keszitettem, es ha barmilyen segitseget igenybe vettem vagy
// mas szellemi termeket felhasznaltam, akkor a forrast es az atvett reszt kommentekben egyertelmuen jeloltem.
// A forrasmegjeloles kotelme vonatkozik az eloadas foliakat es a targy oktatoi, illetve a
// grafhazi doktor tanacsait kiveve barmilyen csatornan (szoban, irasban, Interneten, stb.) erkezo minden egyeb
// informaciora (keplet, program, algoritmus, stb.). Kijelentem, hogy a forrasmegjelolessel atvett reszeket is ertem,
// azok helyessegere matematikai bizonyitast tudok adni. Tisztaban vagyok azzal, hogy az atvett reszek nem szamitanak
// a sajat kontribucioba, igy a feladat elfogadasarol a tobbi resz mennyisege es minosege alapjan szuletik dontes.
// Tudomasul veszem, hogy a forrasmegjeloles kotelmenek megsertese eseten a hazifeladatra adhato pontokat
// negativ elojellel szamoljak el es ezzel parhuzamosan eljaras is indul velem szemben.
//=============================================================================================
#include "framework.h"

// based on:
// http://cg.iit.bme.hu/portal/sites/default/files/oktatott%20t%C3%A1rgyak/sz%C3%A1m%C3%ADt%C3%B3g%C3%A9pes%20grafika/sug%C3%A1rk%C3%B6vet%C3%A9s/raytrace_0.cpp

// vektorgrafikai alapok
// https://cs.oberlin.edu/~bob/cs357.08/VectorGeometry/VectorGeometry.pdf

// vertex shader in GLSL
const char *vertexSource = R"(
	#version 330
    precision highp float;

	uniform vec3 wLookAt, wRight, wUp;          // pos of eye

	layout(location = 0) in vec2 cCamWindowVertex;	// Attrib Array 0
	out vec3 p;

	void main() {
		gl_Position = vec4(cCamWindowVertex, 0, 1);
		p = wLookAt + wRight * cCamWindowVertex.x + wUp * cCamWindowVertex.y;
	}
)";
// fragment shader in GLSL
const char *fragmentSource =R"(
    #version 330
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
        int mat;
    };

    struct Hit {
        float t;
        vec3 position, normal;
        int mat;
    };

    struct Ray {
        vec3 start, dir;
    };

    const int nMaxEllipsoid = 10;
    const int nMaxMirror = 100;

    uniform vec3 wEye;
    uniform Light light;
    uniform Material materials[4];

    uniform Plane bottom;

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
        hit.normal = normalize(object.normal);
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
        vec3 helper = hit.position - object.center;
        hit.normal = normalize(2.0 * vec3(
            helper.x / object.params.x / object.params.x,
            helper.y / object.params.y / object.params.y,
            helper.z / object.params.z / object.params.z
        ));
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
        Hit hitBottom = intersect(bottom, ray); //  hit.t < 0 if no intersection
        if (hitBottom.t > 0 && (bestHit.t < 0 || hitBottom.t < bestHit.t))  bestHit = hitBottom;
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
    const int maxdepth = 8;

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
)";

class Material {
protected:
    vec3 ka, kd, ks;
    float  shininess;
    vec3 F0;
    bool rough, reflective;
public:
    void SetUniform(unsigned int shaderProg, int mat) {
        char buffer[256];
        sprintf(buffer, "materials[%d].ka", mat);
        ka.SetUniform(shaderProg, buffer);
        sprintf(buffer, "materials[%d].kd", mat);
        kd.SetUniform(shaderProg, buffer);
        sprintf(buffer, "materials[%d].ks", mat);
        ks.SetUniform(shaderProg, buffer);
        sprintf(buffer, "materials[%d].shininess", mat);
        int location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1f(location, shininess); else printf("uniform material.shininess cannot be set\n");
        sprintf(buffer, "materials[%d].F0", mat);
        F0.SetUniform(shaderProg, buffer);

        sprintf(buffer, "materials[%d].rough", mat);
        location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, rough ? 1 : 0); else printf("uniform material.rough cannot be set\n");
        sprintf(buffer, "materials[%d].reflective", mat);
        location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, reflective ? 1 : 0); else printf("uniform material.reflective cannot be set\n");
    }
};

class RoughMaterial : public Material {
public:
    RoughMaterial(vec3 _kd, vec3 _ks, float _shininess) {
        ka = _kd * M_PI;
        kd = _kd;
        ks = _ks;
        shininess = _shininess;
        rough = true;
        reflective = false;
    }
};

float rnd() { return (float)rand() / RAND_MAX; }
float randcoord() {
    return (rnd() - 0.5f) * 0.09f;
}
vec3 randmove() {
    return {randcoord(),randcoord(),0};
}

struct Ellipsoid {
    vec3 center;
    const float a, b, c;
    int mat = 0;

    Ellipsoid(const vec3& _center, float _a, float _b, float _c) :
        center(_center), a(_a), b(_b), c(_c) {
    }

    void SetUniform(unsigned int shaderProg, int o) {
        char buffer[256];
        sprintf(buffer, "ellipsoid[%d].center", o);
        center.SetUniform(shaderProg, buffer);
        sprintf(buffer, "ellipsoid[%d].params", o);
        int location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0)
            glUniform3f(location, a, b, c);
        else
            printf("uniform %s cannot be set\n", buffer);
        sprintf(buffer, "ellipsoid[%d].mat", o);
        location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, mat);
        else printf("uniform ellipsoid mat cannot be set\n");
    }

    void move() {
        center = center + randmove();
        if (length(vec2{center.x, center.y}) > 1.0f)
            center = vec3{normalize(vec2{center.x, center.y}).x,normalize(vec2{center.x, center.y}).y, center.z};
    }
};

struct Plane {
    vec3 point;
    vec3 normal;
    int mat = 2;

    Plane(vec3 _point, vec3 _normal): point(_point), normal(_normal) {
        normal = normalize(normal);
    }

    virtual void SetUniform(unsigned int shaderProg, int o) = 0;
};

struct Mirror : public Plane {
    Mirror(vec3 _point, vec3 _normal) : Plane(_point, _normal) {}
    void SetUniform(unsigned int shaderProg, int o) override {
        char buffer[256];
        sprintf(buffer, "mirrors[%d].point", o);
        point.SetUniform(shaderProg, buffer);
        sprintf(buffer, "mirrors[%d].normal", o);
        normal.SetUniform(shaderProg, buffer);
        sprintf(buffer, "mirrors[%d].mat", o);
        int location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, mat);
        else printf("uniform mirror mat cannot be set\n");
    }
};

struct Bottom : public Plane {
    Bottom(vec3 _point, vec3 _normal) : Plane(_point, _normal) {}
    virtual void SetUniform(unsigned int shaderProg, int o = 0) override {
        point.SetUniform(shaderProg, "bottom.point");
        normal.SetUniform(shaderProg, "bottom.normal");
        char buffer[256];
        sprintf(buffer, "bottom.mat", o);
        int location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, 1);
        else printf("uniform bottom mat cannot be set\n");
    }
};

class SmoothMaterial : public Material {
public:
    explicit SmoothMaterial(vec3 _F0) {
        F0 = _F0;
        rough = false;
        reflective = true;
    }
};

class Camera {
    vec3 eye, lookat, right, up;
    float fov;
public:
    void set(vec3 _eye, vec3 _lookat, vec3 vup, double _fov) {
        eye = _eye;
        lookat = _lookat;
        fov = _fov;
        vec3 w = eye - lookat;
        float f = length(w);
        right = normalize(cross(vup, w)) * f * tan(fov / 2);
        up = normalize(cross(w, right)) * f * tan(fov / 2);
    }
    void SetUniform(unsigned int shaderProg) {
        eye.SetUniform(shaderProg, "wEye");
        lookat.SetUniform(shaderProg, "wLookAt");
        right.SetUniform(shaderProg, "wRight");
        up.SetUniform(shaderProg, "wUp");
    }
};

struct Light {
    vec3 direction;
    vec3 Le, La;
    Light(vec3 _direction, vec3 _Le, vec3 _La) {
        direction = normalize(_direction);
        Le = _Le; La = _La;
    }
    void SetUniform(unsigned int shaderProg) {
        La.SetUniform(shaderProg, "light.La");
        Le.SetUniform(shaderProg, "light.Le");
        direction.SetUniform(shaderProg, "light.direction");
    }
};

// komlex szamok alapjan
// http://cg.iit.bme.hu/portal/sites/default/files/oktatott%20t%C3%A1rgyak/sz%C3%A1m%C3%ADt%C3%B3g%C3%A9pes%20grafika/geometri%C3%A1k%20%C3%A9s%20algebr%C3%A1k/bmegeom.pdf
vec2 forgatas(vec2 &point, float angle, vec2 &pivot) {
    vec2 eltolt1 = point - pivot;
    vec2 forgas(cosf(angle), sinf(angle));
    return vec2(
            eltolt1.x * forgas.x - eltolt1.y * forgas.y,
            eltolt1.x * forgas.y + eltolt1.y * forgas.x
    ) + pivot;
}

float F0(float n, float k) {
    float counter = (n-1)*(n-1) + k*k;
    float div = (n+1)*(n+1) + k*k;
    return counter / div;
}

class Scene {
    std::vector<Ellipsoid *> ellipsoids;
    Camera camera;
    std::vector<Material *> materials;
    std::vector<Light *> lights;
    Bottom *bottom;
    std::vector<Mirror *> mirrors;
    unsigned sides = 3;
    static const unsigned maxSide = 100;
    bool built = false;
    bool mirrorChanged = true;
public:
    void build() {
        vec3 eye = vec3(0, 0, 10);
        vec3 vup = vec3(0, 1, 0);
        vec3 lookat = vec3(0, 0, 0);
        float fov = 45 * M_PI / 180;
        camera.set(eye, lookat, vup, fov);

        lights.push_back(new Light(vec3(0, 0, 1), vec3(0.8, 0.8, 0.8), vec3(0.2, 0.2, 0.2)));

        for (int i=0; i<5; ++i) {
            float a = rnd() * 0.05 + 0.15;
            float b = rnd() * 0.05 + 0.15;
            float c = rnd() * 0.05 + 0.15;
            vec3 center = {rnd() - 0.5f, rnd() - 0.5f, -4.0f};
            ellipsoids.push_back(new Ellipsoid(center, a, b, c));
        }

        bottom = new Bottom({0, 0, -5}, {0, 0, 1});

        for (int i=0; i<sides; ++i){
            vec2 point{0, 1}, origo{0, 0};
            float angle = (float)i/sides * M_PI * 2;
            point = forgatas(point, angle, origo);
            mirrors.push_back(new Mirror(
                    {point.x, point.y, 0},
                    {-1*point.x, -1*point.y, 0}
                    ));
        }

        vec3 kd(1, 0, 0), ks(1, 1, 1);
        vec3 kd1(0.6, 0.6, 0.6), ks1(0, 0, 0);

        materials.push_back(new RoughMaterial(kd, ks, 25));
        materials.push_back(new RoughMaterial(kd1, ks1, 0));
        materials.push_back(new SmoothMaterial(vec3(F0(0.17, 3.1), F0(0.35,2.7), F0(1.5,1.9))));
        materials.push_back(new SmoothMaterial(vec3(F0(0.14,4.1), F0(0.16,2.3), F0(0.13,3.1))));
        built = true;
    }

    void firstSetUniform(unsigned int shaderProg) {
        camera.SetUniform(shaderProg);
        int location = glGetUniformLocation(shaderProg, "nEllipsoid");
        if (location >= 0) glUniform1i(location, ellipsoids.size());
        else printf("uniform nEllipsoid cannot be set\n");
        for (int mat = 0; mat < materials.size(); mat++) materials[mat]->SetUniform(shaderProg, mat);
        lights[0]->SetUniform(shaderProg);
    }

    void SetUniform(unsigned int shaderProg) {

        for (int i=0; i<ellipsoids.size(); ++i)
            ellipsoids[i]->SetUniform(shaderProg, i);

        if (mirrorChanged) {
            int location = glGetUniformLocation(shaderProg, "nMirror");
            if (location >= 0) glUniform1i(location, mirrors.size());
            else printf("uniform nMirror cannot be set\n");

            for (int i=0; i<mirrors.size(); ++i)
                mirrors[i]->SetUniform(shaderProg, i);
            bottom->SetUniform(shaderProg);
            mirrorChanged = false;
        }
    }

    void increaseMirror() {
        if (sides < maxSide) {
            mirrorChanged = true;
            sides++;
            int i=0;
            for (; i<sides-1; ++i){
                vec2 point{0, 1}, origo{0, 0};
                float angle = (float)i/sides * M_PI * 2;
                point = forgatas(point, angle, origo);
                *mirrors[i] = Mirror(
                        {point.x, point.y, 0},
                        {-1*point.x, -1*point.y, 0}
                );
            }
            vec2 point{0, 1}, origo{0, 0};
            float angle = (float)i/sides * M_PI * 2;
            point = forgatas(point, angle, origo);
            mirrors.push_back(new Mirror(
                    {point.x, point.y, 0},
                    {-1*point.x, -1*point.y, 0}
            ));
        }
    }

    void changeMirrorMaterial(int shaderProg, unsigned materialId){
        for (auto & mirror : mirrors) {
            mirror->mat = materialId;
        }
        mirrorChanged=true;
    }

    void Animate() {
        for (auto & ellipsoid: ellipsoids)
            ellipsoid->move();
    }

    ~Scene() {
        if (built) {
            for (auto & material: materials)
                delete material;
            for (auto & ellipsoid: ellipsoids)
                delete ellipsoid;
            for (auto & mirror: mirrors)
                delete mirror;
            delete bottom;
        }
    }
};

GPUProgram gpuProgram; // vertex and fragment shaders
Scene scene;

class FullScreenTexturedQuad {
    unsigned int vao;	// vertex array object id and texture id
public:
    void Create() {
        glGenVertexArrays(1, &vao);	// create 1 vertex array object
        glBindVertexArray(vao);		// make it active

        unsigned int vbo;		// vertex buffer objects
        glGenBuffers(1, &vbo);	// Generate 1 vertex buffer objects

        // vertex coordinates: vbo0 -> Attrib Array 0 -> vertexPosition of the vertex shader
        glBindBuffer(GL_ARRAY_BUFFER, vbo); // make it active, it is an array
        float vertexCoords[] = { -1, -1,  1, -1,  1, 1,  -1, 1 };	// two triangles forming a quad
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertexCoords), vertexCoords, GL_STATIC_DRAW);	   // copy to that part of the memory which is not modified
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);     // stride and offset: it is tightly packed
    }

    void Draw() {
        glBindVertexArray(vao);	// make the vao and its vbos active playing the role of the data source
        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);	// draw two triangles forming a quad
    }
};

FullScreenTexturedQuad fullScreenTexturedQuad;

// Initialization, create an OpenGL context
void onInitialization() {
    glViewport(0, 0, windowWidth, windowHeight);
    scene.build();
    fullScreenTexturedQuad.Create();

    gpuProgram.Create(vertexSource, fragmentSource, "fragmentColor");
    gpuProgram.Use();
    scene.firstSetUniform(gpuProgram.getId());
}

// Window has become invalid: Redraw
void onDisplay() {
    static int nFrames = 0;
    nFrames++;
    static long tStart = glutGet(GLUT_ELAPSED_TIME);
    long tEnd = glutGet(GLUT_ELAPSED_TIME);
    printf("%d msec\r", (tEnd - tStart) / nFrames);

    glClearColor(1.0f, 0.5f, 0.8f, 1.0f);							// background color
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // clear the screen
    scene.SetUniform(gpuProgram.getId());
    fullScreenTexturedQuad.Draw();
    glutSwapBuffers();									// exchange the two buffers
}

// Key of ASCII code pressed
void onKeyboard(unsigned char key, int pX, int pY) {
}

// Key of ASCII code released
void onKeyboardUp(unsigned char key, int pX, int pY) {
    switch (key) {
        case 'a':
            scene.increaseMirror();
            break;
        case 'g':
            scene.changeMirrorMaterial(gpuProgram.getId(), 2);
            break;
        case 's':
            scene.changeMirrorMaterial(gpuProgram.getId(), 3);
            break;
    }
}

// Mouse click event
void onMouse(int button, int state, int pX, int pY) {
}

// Move mouse with key pressed
void onMouseMotion(int pX, int pY) {
}

// Idle event indicating that some time elapsed: do animation here
void onIdle() {
    scene.Animate();
    glutPostRedisplay();
}