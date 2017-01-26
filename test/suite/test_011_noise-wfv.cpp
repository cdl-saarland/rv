// Shapes: T_TrT, LaunchCode: foo2f8
// test_085_noise(float x, float y)

#include <cmath>

//array access
extern "C" float
foo(float x, float y)
{
    float z = x*y;
    static int p[] = { 151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180,
        151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
    };

    //here begins 'real' noise-function
    const int X = (int)floorf(x) & 255;                  // FIND UNIT CUBE THAT
    const int Y = (int)floorf(y) & 255;                  // CONTAINS POINT.
    const int Z = (int)floorf(z) & 255;
    x -= floorf(x);                                      // FIND RELATIVE X,Y,Z
    y -= floorf(y);                                      // OF POINT IN CUBE.
    z -= floorf(z);
    const float U = x * x * x * (x * (x * 6 - 15) + 10);  // COMPUTE FADE CURVES
    const float V = y * y * y * (y * (y * 6 - 15) + 10);  // FOR EACH OF X,Y,Z.
    const float W = z * z * z * (z * (z * 6 - 15) + 10);
    const int A = p[X  ]+Y, AA = p[A]+Z, AB = p[A+1]+Z;   // HASH COORDINATES OF
    const int B = p[X+1]+Y, BA = p[B]+Z, BB = p[B+1]+Z;   // THE 8 CUBE CORNERS,

//      return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  // AND ADD
//                                     grad(p[BA  ], x-1, y  , z   )), // BLENDED
//                             lerp(u, grad(p[AB  ], x  , y-1, z   ),  // RESULTS
//                                     grad(p[BB  ], x-1, y-1, z   ))),// FROM  8
//                     lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  // CORNERS
//                                     grad(p[BA+1], x-1, y  , z-1 )), // OF CUBE
//                             lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
//                                     grad(p[BB+1], x-1, y-1, z-1 ))));

    int h = p[AA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    float u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    float v = h<4 ? y : h==12||h==14 ? x : z;
    const float grad1 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad1 = grad(p[AA  ], x  , y  , z   );

    h = p[BA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z;
    const float grad2 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad2 = grad(p[BA  ], x-1, y  , z   );

    h = p[AB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z;
    const float grad3 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad3 = grad(p[AB  ], x  , y-1, z   );

    h = p[BB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z;
    const float grad4 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad4 = grad(p[BB  ], x-1, y-1, z   );

    h = p[AA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x : z-1;
    const float grad5 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad5 = grad(p[AA+1], x  , y  , z-1 );

    h = p[BA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z-1;
    const float grad6 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad6 = grad(p[BA+1], x-1, y  , z-1 );

    h = p[AB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z-1;
    const float grad7 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad7 = grad(p[AB+1], x  , y-1, z-1 );

    h = p[BB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z-1;
    const float grad8 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad8 = grad(p[BB+1], x-1, y-1, z-1 );

    const float lerp1 = grad1 + U * (grad2 - grad1); //lerp(u, grad1, grad2);
    const float lerp2 = grad3 + U * (grad4 - grad3); //lerp(u, grad3, grad4);
    const float lerp3 = grad5 + U * (grad6 - grad5); //lerp(u, grad5, grad6);
    const float lerp4 = grad7 + U * (grad8 - grad7); //lerp(u, grad7, grad8);
    const float lerp12 = lerp1 + V * (lerp2 - lerp1); //lerp(v, lerp1, lerp2);
    const float lerp34 = lerp3 + V * (lerp4 - lerp3); //lerp(v, lerp3, lerp4);
    return lerp12 + W * (lerp34 - lerp12); //lerp(w, lerp12, lerp34);
}

