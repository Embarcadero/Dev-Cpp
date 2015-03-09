/* 
   Name: WinAnim
   Author: Brook Miles
   Description: Making an animation in windows
*/

#include <windows.h>

static char g_szClassName[] = "MyWindowClass";
static HINSTANCE g_hInst = NULL;

const UINT idTimer1 = 1;
UINT nTimerDelay = 10;

HBITMAP hbmBall, hbmMask;
BITMAP bm;

int ballX, ballY;
int deltaX, deltaY;

int deltaValue = 4;

void EraseBall(HDC hdc)
{
   RECT rc;
   rc.left = ballX;
   rc.top = ballY;
   rc.right = ballX + bm.bmWidth;
   rc.bottom = ballY + bm.bmHeight;
   FillRect(hdc, &rc, (HBRUSH)(COLOR_BTNFACE+1));
}

void DrawBall(HDC hdc)
{
   HDC hdcMemory;
   hdcMemory = CreateCompatibleDC(hdc);

   SelectObject(hdcMemory, hbmMask);
   BitBlt(hdc, ballX, ballY, bm.bmWidth, bm.bmHeight, hdcMemory, 0, 0, SRCAND);

   SelectObject(hdcMemory, hbmBall);
   BitBlt(hdc, ballX, ballY, bm.bmWidth, bm.bmHeight, hdcMemory, 0, 0, SRCPAINT);

   DeleteDC(hdcMemory);
}

void UpdateBall(HWND hwnd)
{
   RECT rc;
   GetClientRect(hwnd, &rc);

   ballX += deltaX;
   ballY += deltaY;

   if(ballX < 0){
      ballX = 0;
      deltaX = deltaValue;
   }
   else if(ballX + bm.bmWidth > rc.right){
      ballX = rc.right - bm.bmWidth;
      deltaX = -deltaValue;
   }
   if(ballY < 0){
      ballY = 0;
      deltaY = deltaValue;
   }
   else if(ballY + bm.bmHeight > rc.bottom){
      ballY = rc.bottom - bm.bmHeight;
      deltaY = -deltaValue;
   }
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam)
{
   switch(Message)
   {
      case WM_CREATE:
         hbmBall = LoadBitmap(g_hInst, "BALLBMP");
         hbmMask = LoadBitmap(g_hInst, "MASKBMP");
         if(!hbmBall || !hbmMask){
            MessageBox(hwnd, "Load of resources failed.", "Error",
               MB_OK | MB_ICONEXCLAMATION);
            return -1;
         }

         GetObject(hbmBall, sizeof(bm), &bm);
         SetTimer(hwnd, idTimer1, nTimerDelay, NULL);

         ballX = 0;
         ballY = 0;
         deltaX = deltaValue;
         deltaY = deltaValue;

      break;
      case WM_TIMER:
         if(hbmBall && hbmMask)
         {
            HDC hdcWindow;
            hdcWindow = GetDC(hwnd);

            EraseBall(hdcWindow);
            UpdateBall(hwnd);
            DrawBall(hdcWindow);

            ReleaseDC(hwnd, hdcWindow);
         }
      break;
      case WM_PAINT:
         if(hbmBall && hbmMask)
         {
            PAINTSTRUCT ps;
            HDC hdcWindow;
            hdcWindow = BeginPaint(hwnd, &ps);

            DrawBall(hdcWindow);
            
            EndPaint(hwnd, &ps);
         }
      break;
      case WM_CLOSE:
         DestroyWindow(hwnd);
      break;
      case WM_DESTROY:
         KillTimer(hwnd, idTimer1);
         
         DeleteObject(hbmBall);
         DeleteObject(hbmMask);
         PostQuitMessage(0);
      break;
      default:
         return DefWindowProc(hwnd, Message, wParam, lParam);
   }
   return 0;
}


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
   LPSTR lpCmdLine, int nCmdShow)
{
   WNDCLASSEX WndClass;
   HWND hwnd;
   MSG Msg;

   g_hInst = hInstance;

   WndClass.cbSize        = sizeof(WNDCLASSEX);
   WndClass.style         = 0;
   WndClass.lpfnWndProc   = WndProc;
   WndClass.cbClsExtra    = 0;
   WndClass.cbWndExtra    = 0;
   WndClass.hInstance     = g_hInst;
   WndClass.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
   WndClass.hCursor       = LoadCursor(NULL, IDC_ARROW);
   WndClass.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1);
   WndClass.lpszMenuName  = NULL;
   WndClass.lpszClassName = g_szClassName;
   WndClass.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);

   if(!RegisterClassEx(&WndClass))
   {
      MessageBox(0, "Window Registration Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      return 0;
   }

   hwnd = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      g_szClassName,
      "A Bitmap Program",
      WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, 320, 240,
      NULL, NULL, g_hInst, NULL);

   if(hwnd == NULL)
   {
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      return 0;
   }

   ShowWindow(hwnd, nCmdShow);
   UpdateWindow(hwnd);

   while(GetMessage(&Msg, NULL, 0, 0))
   {
      TranslateMessage(&Msg);
      DispatchMessage(&Msg);
   }
   return Msg.wParam;
}

