function draw_octahedron()
% DRAW_OCTAHEDRON Plots a 3D octahedron centered at the origin.
%
%   The octahedron is defined by 6 vertices along the x, y, and z axes.

    disp('Drawing a 3D Octahedron. Please wait for the figure window.');

    % 1. Define the 6 Vertices (V)
    % Vertices are positioned on the positive and negative axes.
    % V = [X, Y, Z]
    V = [
        1,  0,  0;  % V1: +X axis
       -1,  0,  0;  % V2: -X axis
        0,  1,  0;  % V3: +Y axis
        0, -1,  0;  % V4: -Y axis
        0,  0,  1;  % V5: +Z axis
        0,  0, -1   % V6: -Z axis
    ];

    % 2. Define the 8 Faces (F)
    % Each row in F lists the indices of the vertices (1-6) that form a triangular face.
    F = [
        % Top 4 faces (+Z side)
        1, 3, 5;    % V1 (+X), V3 (+Y), V5 (+Z)
        3, 2, 5;    % V3 (+Y), V2 (-X), V5 (+Z)
        2, 4, 5;    % V2 (-X), V4 (-Y), V5 (+Z)
        4, 1, 5;    % V4 (-Y), V1 (+X), V5 (+Z)
        
        % Bottom 4 faces (-Z side)
        1, 3, 6;    % V1 (+X), V3 (+Y), V6 (-Z)
        3, 2, 6;    % V3 (+Y), V2 (-X), V6 (-Z)
        2, 4, 6;    % V2 (-X), V4 (-Y), V6 (-Z)
        4, 1, 6     % V4 (-Y), V1 (+X), V6 (-Z)
    ];

    % 3. Setup the Figure
    figure;
    
    % 4. Use the PATCH function to draw the 3D object
    patch('Vertices', V, 'Faces', F, ...
          'FaceColor', [0.3 0.6 0.9], ... % Orange/Brown color
          'EdgeColor', 'k', ...         % Black edges
          'LineWidth', 1.5, ...
          'FaceAlpha', 0.4);            % Slightly transparent
    
    % 5. Set Axes Properties for a clear view
    axis equal;                 % Ensure equal scaling in all directions
    axis([-1.25 1.25 -1.25 1.25 -1.25 1.25]);
    grid on;
    view(3);                    % Set the standard 3D perspective view
    camlight;                   % Add lighting to make it look 3D
    lighting gouraud;           % Smooth shading
    
    % 6. Add labels and title
    title('S^2 triangulation');
    xlabel('X-axis');
    ylabel('Y-axis');
    zlabel('Z-axis');

end