//=============================================================================================
// Mintaprogram: Zöld háromszög. Ervenyes 2018. osztol.
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
// Nev    : Stork Gábor
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
	#version 450
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
const char *fragmentSource =
        #include "valami.glsl"
                ;

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

struct Ellipsoid {
    vec3 center;
    const float a, b, c;
    vec3 color;
    int mat = 0;

    Ellipsoid(const vec3& _center, float _a, float _b, float _c, vec3 _color) :
        center(_center), a(_a), b(_b), c(_c), color{_color} {
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
        sprintf(buffer, "ellipsoid[%d].color", o);
        color.SetUniform(shaderProg, buffer);
        sprintf(buffer, "ellipsoid[%d].mat", o);
        location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, mat);
        else printf("uniform ellipsoid mat cannot be set\n");
    }
};

struct Plane {
    vec3 point;
    vec3 normal;
    int mat = 1;

    Plane(vec3 _point, vec3 _normal): point(_point), normal(_normal) {}

    virtual void SetUniform(unsigned int shaderProg, int o) = 0;
};

struct Mirror : public Plane {
    using Plane::Plane;
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
    using Plane::Plane;
    virtual void SetUniform(unsigned int shaderProg, int o = 0) override {
        point.SetUniform(shaderProg, "bottom.point");
        normal.SetUniform(shaderProg, "bottom.normal");
        char buffer[256];
        sprintf(buffer, "bottom.mat", o);
        int location = glGetUniformLocation(shaderProg, buffer);
        if (location >= 0) glUniform1i(location, mat);
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

float rnd() { return (float)rand() / RAND_MAX; }

// komlex számok alapján
// http://cg.iit.bme.hu/portal/sites/default/files/oktatott%20t%C3%A1rgyak/sz%C3%A1m%C3%ADt%C3%B3g%C3%A9pes%20grafika/geometri%C3%A1k%20%C3%A9s%20algebr%C3%A1k/bmegeom.pdf
vec2 forgatas(vec2 &point, float angle, vec2 &pivot) {
    vec2 eltolt1 = point - pivot;
    vec2 forgas(cosf(angle), sinf(angle));
    return vec2(
            eltolt1.x * forgas.x - eltolt1.y * forgas.y,
            eltolt1.x * forgas.y + eltolt1.y * forgas.x
    ) + pivot;
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
        vec3 eye = vec3(0, 0, 2);
        vec3 vup = vec3(0, 1, 0);
        vec3 lookat = vec3(0, 0, 0);
        float fov = 45 * M_PI / 180;
        camera.set(eye, lookat, vup, fov);

        lights.push_back(new Light(vec3(1, 1, 1), vec3(3, 3, 3), vec3(0.4, 0.3, 0.3)));

        // add 4-5 ellipsoid
        for (int i=0; i<5; ++i) {
            float a = rnd() * 0.2 + 0.05;
            float b = rnd() * 0.2 + 0.05;
            float c = rnd() * 0.2 + 0.05;
            vec3 color = {1,0,0};
            vec3 center = {rnd(), rnd(), -3 + c};
            ellipsoids.push_back(new Ellipsoid(center, a, b, c, color));
        }

        bottom = new Bottom({0, 0, -3}, {0, 0, 1});

        for (int i=0; i<sides; ++i){
            vec2 point{0, 1}, origo{0, 0};
            float angle = (float)i/sides * M_PI * 2;
            point = forgatas(point, angle, origo);
            mirrors.push_back(new Mirror(
                    {point.x, point.y, 0},
                    {-1*point.x, -1*point.y, 0}
                    ));
        }

        vec3 kd(0.3f, 0.2f, 0.1f), ks(10, 10, 10);
        materials.push_back(new RoughMaterial(kd, ks, 50));
        materials.push_back(new SmoothMaterial(vec3(0.9, 0.85, 0.8)));

        built = true;
    }

    void SetUniform(unsigned int shaderProg) {
        camera.SetUniform(shaderProg);

        int location = glGetUniformLocation(shaderProg, "nEllipsoid");
        if (location >= 0) glUniform1i(location, ellipsoids.size());
        else printf("uniform nEllipsoid cannot be set\n");
        for (int i=0; i<ellipsoids.size(); ++i)
            ellipsoids[i]->SetUniform(shaderProg, i);

        lights[0]->SetUniform(shaderProg);

        //if (mirrorChanged) {
            location = glGetUniformLocation(shaderProg, "nMirror");
            if (location >= 0) glUniform1i(location, mirrors.size());
            else printf("uniform nMirror cannot be set\n");

            for (int i=0; i<mirrors.size(); ++i)
                mirrors[i]->SetUniform(shaderProg, i);
            bottom->SetUniform(shaderProg);
            mirrorChanged = false;
        //}
        for (int mat = 0; mat < materials.size(); mat++) materials[mat]->SetUniform(shaderProg, mat);
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

    void changeMirrorMaterial(unsigned int shaderProg, unsigned materialId){
        int location = glGetUniformLocation(shaderProg, "mirrorMaterial");
        if (location >= 0) glUniform1i(location, materialId);
        else printf("uniform mirrorMaterial cannot be set\n");
    }

    void Animate(float dt) {

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

    // create program for the GPU
    gpuProgram.Create(vertexSource, fragmentSource, "fragmentColor");
    gpuProgram.Use();
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

}

// Mouse click event
void onMouse(int button, int state, int pX, int pY) {
}

// Move mouse with key pressed
void onMouseMotion(int pX, int pY) {
}

// Idle event indicating that some time elapsed: do animation here
void onIdle() {
    scene.Animate(0.01);
    glutPostRedisplay();
}