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
const char *fragmentSource = R"(
	#version 450
    precision highp float;

    struct Material {
		vec3 ka, kd, ks;
		float  shininess;
		vec3 F0;
		int rough, reflective;
	};

    struct Plane {
        vec3 point;
        vec3 normal;
    };

    struct Ellipsoid {
		vec3 center;
		vec3 params;
	};

	const int nMaxEllipsoid = 10;
    const int nMaxMirror = 100;

	uniform vec3 wEye;
    uniform Material materials[2]; // TODO

    uniform Plane bottom;

    uniform int mirrorMaterial;
    uniform int nMirror;
    uniform Plane mirrors[nMaxMirror];

	uniform int nEllipsoid;
    uniform Ellipsoid ellipsoid[nMaxEllipsoid];

	in  vec3 p;					// point on camera window corresponding to the pixel
	out vec4 fragmentColor;		// output that goes to the raster memory as told by glBindFragDataLocation

    vec3 Fresnel(vec3 F0, float cosTheta) {
		return F0 + (vec3(1, 1, 1) - F0) * pow(cosTheta, 5);
	}

    const float epsilon = 0.0001f;
	const int maxdepth = 5;

	void main() {
		fragmentColor = vec4(1, 0, 0, 1);
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

struct Ellipsoid {
    vec3 center;
    const float a, b, c;

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

    }
};

struct Plane {
    vec3 point;
    vec3 normal;

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
    }
};

struct Bottom : public Plane {
    using Plane::Plane;
    virtual void SetUniform(unsigned int shaderProg, int o = 0) override {
        point.SetUniform(shaderProg, "bottom.point");
        normal.SetUniform(shaderProg, "bottom.normal");
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

class Scene {
    std::vector<Ellipsoid *> ellipsoids;
    Camera camera;
    std::vector<Material *> materials;
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

        // add 4-5 ellipsoid

        bottom = new Bottom({0, 0, 0}, {0, 0, 1});

        // add mirrors
        built = true;
    }

    void SetUniform(unsigned int shaderProg) {
        camera.SetUniform(shaderProg);

        for (int i=0; i<ellipsoids.size(); ++i)
            ellipsoids[i]->SetUniform(shaderProg, i);

        //if (mirrorChanged) {
            int location = glGetUniformLocation(shaderProg, "nMirrors");
            if (location >= 0) glUniform1i(location, mirrors.size());
            else printf("uniform nMirrors cannot be set\n");

            for (int i=0; i<mirrors.size(); ++i)
                mirrors[i]->SetUniform(shaderProg, i);
            bottom->SetUniform(shaderProg);
            mirrorChanged = false;
        //}

    }

    void increaseMirror() {
        if (sides < maxSide) {
            sides++;
            mirrorChanged = true;
            // TODO add mirror, change the others
        }
    }

    void changeMirrorMaterial(unsigned materialId){
        // TODO change uniform
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