rng("default")
c=cos(pi/4);
p0 = [0,0,0];
p1 = [c,c,0.];
p2 = [c,0,c];
p3 = [0,c,c];
norm(p2-p3)
X=[p0;p1;p2;p3]
dt = delaunayTriangulation(X);


rng("default")
X = -3 + 6.*rand([25 3]);
dt = delaunayTriangulation(X);
[V,R] = voronoiDiagram(dt);

tid = nearestNeighbor(dt,0,0,0);
XR10 = V(R{tid},:);
K = convhull(XR10);
defaultFaceColor = [0.6875 0.8750 0.8984];
trisurf(K,XR10(:,1),XR10(:,2),XR10(:,3) , ...
        FaceColor=defaultFaceColor,FaceAlpha=0.8)
title("3-D Voronoi Region")