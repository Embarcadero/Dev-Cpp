typedef struct _D3DVECTOR {
    float x;
    float y;
    float z;
} D3DVECTOR;

typedef struct D3DXVECTOR3 : public D3DVECTOR
{
    D3DXVECTOR3();
    D3DXVECTOR3(const FLOAT *pf);
    D3DXVECTOR3(const D3DVECTOR& v);
    D3DXVECTOR3(FLOAT fx, FLOAT fy, FLOAT fz);

    operator FLOAT* ();
    operator const FLOAT* () const;

    D3DXVECTOR3& operator += (const D3DXVECTOR3&);
    D3DXVECTOR3& operator -= (const D3DXVECTOR3&);
    D3DXVECTOR3& operator *= (FLOAT);
    D3DXVECTOR3& operator /= (FLOAT);

    D3DXVECTOR3 operator + () const;
    D3DXVECTOR3 operator - () const;

    D3DXVECTOR3 operator + (const D3DXVECTOR3&) const;
    D3DXVECTOR3 operator - (const D3DXVECTOR3&) const;
    D3DXVECTOR3 operator * (FLOAT) const;
    D3DXVECTOR3 operator / (FLOAT) const;

    friend D3DXVECTOR3 operator * (FLOAT, const struct D3DXVECTOR3&);

    WINBOOL operator == (const D3DXVECTOR3&) const;
    WINBOOL operator != (const D3DXVECTOR3&) const;
} D3DXVECTOR3, *LPD3DXVECTOR3;

class float3 : public D3DXVECTOR3 {
};

