// make sure to link to the libraries libd3d9.a and libd3dx9_43.a
// - copy the two files from the lib directory of your compiler
// - open project options >> parameters and add both using "Add Library or Object"

#include <windows.h>
#include <windowsx.h>
#include <d3d9.h>
#include <d3dx9.h>

// Defines
struct CUSTOMVERTEX {
	float X;
	float Y;
	float Z;
	DWORD COLOR;
};
#define CUSTOMFVF (D3DFVF_XYZ | D3DFVF_DIFFUSE)

// global declarations
LPDIRECT3D9 d3d;								// the pointer to our Direct3D interface
LPDIRECT3DDEVICE9 d3ddev;						// the pointer to the device class
LPDIRECT3DVERTEXBUFFER9 vertexbuffer = NULL;	// the pointer to the vertex buffer

// this is the function that puts the 3D models into video RAM
void init_graphics() {
	
    // create the vertices using the CUSTOMVERTEX struct
    CUSTOMVERTEX vertices[] =
    {
        { 3.0f, -3.0f, 0.0f, D3DCOLOR_XRGB(0, 0, 255)},
        { 0.0f,  3.0f, 0.0f, D3DCOLOR_XRGB(0, 255, 0)},
        {-3.0f, -3.0f, 0.0f, D3DCOLOR_XRGB(255, 0, 0)},
    };

    // create a vertex buffer interface called v_buffer
    d3ddev->CreateVertexBuffer(3 * sizeof(CUSTOMVERTEX), 0, CUSTOMFVF, D3DPOOL_MANAGED, &vertexbuffer, NULL);

    VOID* pVoid;

    // lock v_buffer and load the vertices into it
    vertexbuffer->Lock(0, 0, (void**)&pVoid, 0);
    memcpy(pVoid, vertices, sizeof(vertices));
    vertexbuffer->Unlock();
}

// this function initializes and prepares Direct3D for use
void initD3D(HWND hwnd) {
    d3d = Direct3DCreate9(D3D_SDK_VERSION);

    D3DPRESENT_PARAMETERS d3dpp;

    ZeroMemory(&d3dpp, sizeof(d3dpp));
    
    d3dpp.Windowed 					= true;
    d3dpp.SwapEffect 				= D3DSWAPEFFECT_DISCARD;
    d3dpp.hDeviceWindow 			= hwnd;
    d3dpp.BackBufferFormat	 		= D3DFMT_X8R8G8B8;
    d3dpp.BackBufferWidth 			= 800;
    d3dpp.BackBufferHeight 			= 600;
    d3dpp.EnableAutoDepthStencil 	= true;        // Manage SetDepthStencil for us
    d3dpp.AutoDepthStencilFormat 	= D3DFMT_D16;	// 16-bit pixel format for the z-buffer

    // create a device class using this information and the info from the d3dpp stuct
    d3d->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hwnd, D3DCREATE_SOFTWARE_VERTEXPROCESSING, &d3dpp, &d3ddev);

	// Create resources
    init_graphics();

    d3ddev->SetRenderState(D3DRS_LIGHTING, false);    		// turn off the 3D lighting
    d3ddev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);	// both sides of the triangles
    d3ddev->SetRenderState(D3DRS_ZENABLE,  true);			// turn on the z-buffer
}

// this is the function used to render a single frame
void render_frame() {
	
    d3ddev->Clear(0, NULL, D3DCLEAR_TARGET,  D3DCOLOR_XRGB(0, 0, 0), 1.0f, 0);
    d3ddev->Clear(0, NULL, D3DCLEAR_ZBUFFER, D3DCOLOR_XRGB(0, 0, 0), 1.0f, 0);

    d3ddev->BeginScene();

    // select which vertex format we are using
    d3ddev->SetFVF(CUSTOMFVF);

    // set the view transform
    D3DXMATRIX matView;    // the view transform matrix
    D3DXVECTOR3 camPos(0.0f, 0.0f, 15.0f);
    D3DXVECTOR3 lookAt(0.0f, 0.0f, 0.0f);
    D3DXVECTOR3 up(0.0f, 1.0f, 0.0f);
    
    D3DXMatrixLookAtLH(&matView,
                       &camPos,		// the camera position
                       &lookAt,		// the look-at position
                       &up);		// the up direction
    d3ddev->SetTransform(D3DTS_VIEW, &matView);

    // set the projection transform
    D3DXMATRIX matProjection;    // the projection transform matrix
    D3DXMatrixPerspectiveFovLH(&matProjection,
                               D3DXToRadian(45),			// the vertical field of view
                               800.0f/600.0f,			// aspect ratio
                               1.0f,						// the near view-plane
                               100.0f);						// the far view-plane
    d3ddev->SetTransform(D3DTS_PROJECTION, &matProjection);

    // select the vertex buffer to display
    d3ddev->SetStreamSource(0, vertexbuffer, 0, sizeof(CUSTOMVERTEX));

    D3DXMATRIX matTranslateA;    // a matrix to store the translation for triangle A
    D3DXMATRIX matTranslateB;    // a matrix to store the translation for triangle B
    D3DXMATRIX matRotateY;    // a matrix to store the rotation for each triangle
    static float index = 0.0f; index+=0.05f; // an ever-increasing float value

    // build MULTIPLE matrices to translate the model and one to rotate
    D3DXMatrixTranslation(&matTranslateA, 0.0f, 0.0f, 2.0f);
    D3DXMatrixTranslation(&matTranslateB, 0.0f, 0.0f, -2.0f);
    D3DXMatrixRotationY(&matRotateY, index);    // the front side

    // tell Direct3D about each world transform, and then draw another triangle
    D3DMATRIX matTemp(matTranslateA * matRotateY);
    d3ddev->SetTransform(D3DTS_WORLD, &matTemp);
    d3ddev->DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);

	matTemp = matTranslateB * matRotateY;
    d3ddev->SetTransform(D3DTS_WORLD, &matTemp);
    d3ddev->DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);

    d3ddev->EndScene();

    d3ddev->Present(NULL, NULL, NULL, NULL);
}

// this is the function that cleans up Direct3D and COM
void cleanD3D() {
    vertexbuffer->Release();	// close and release the vertex buffer
    d3ddev->Release();		// close and release the 3D device
    d3d->Release();			// close and release Direct3D
}

// this is the main message handler for the program
LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)  {
	switch(message) {
		case WM_DESTROY: {
			PostQuitMessage(0);
			break;
		}
		default:
			return DefWindowProc (hWnd, message, wParam, lParam);
    }
}

// the entry point for any Windows program
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    HWND hwnd;
    WNDCLASSEX wc;
    MSG msg;

    memset(&wc,0,sizeof(wc));
	wc.cbSize		 = sizeof(WNDCLASSEX);
	wc.lpfnWndProc	 = WindowProc;
	wc.hInstance	 = hInstance;
	wc.hCursor		 = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW);
	wc.lpszClassName = "WindowClass";
	wc.hIcon		 = NULL;
	wc.hIconSm		 = NULL;

    RegisterClassEx(&wc);

    hwnd = CreateWindowEx(0, "WindowClass", "Direct3D FFP Example", WS_OVERLAPPEDWINDOW, 0, 0, 800, 600, NULL, NULL, hInstance, NULL);

    ShowWindow(hwnd, SW_SHOW);

    // set up and initialize Direct3D
    initD3D(hwnd);

    // enter the main loop
    while(msg.message != WM_QUIT) {
        while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        render_frame();
    }

    // clean up DirectX and COM
    cleanD3D();

    return msg.wParam;
}
