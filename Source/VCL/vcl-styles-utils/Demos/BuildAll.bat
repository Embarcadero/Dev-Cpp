@Echo OFF
FOR /L %%G IN (12,1,21) DO IF %%G NEQ 16 call Demo_Build.bat %%G
PAUSE