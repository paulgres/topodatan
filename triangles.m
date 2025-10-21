% MATLAB Code to Plot 2D Triangles from Data File

%% 1. Configuration and Data Loading
% Define the name of your data file
data_file = 'triangle_data.txt'; 

% --- IMPORTANT ---
% If your data is exactly as provided in the prompt (6 columns per row),
% and is in a file, use the 'load' function:
% data = load(data_file);

% For demonstration, we use the provided data directly,
% interpreting the second row as a second triangle.
% --- 1. Import the data ---
fileName = 'faces.txt'; % Replace with your actual file name
data = dlmread(fileName); % Reads the file into a matrix

% Correctly reshape the data for plotting:
% N rows (triangles) x 6 columns (x1 y1 x2 y2 x3 y3)
num_triangles = size(data, 1);

%% 2. Prepare Vertex Coordinates
% Initialize matrices to hold all X and Y coordinates for all vertices
% Each row will contain [x1, x2, x3] or [y1, y2, y3]
X_coords = zeros(num_triangles, 3);
Y_coords = zeros(num_triangles, 3);

% Extract the coordinates based on the (x1, y1, x2, y2, x3, y3) format
X_coords(:, 1) = data(:, 1); % x1
X_coords(:, 2) = data(:, 3); % x2
X_coords(:, 3) = data(:, 5); % x3

Y_coords(:, 1) = data(:, 2); % y1
Y_coords(:, 2) = data(:, 4); % y2
Y_coords(:, 3) = data(:, 6); % y3

%% 3. Plot the Triangles using 'patch'

figure; % Create a new figure window
hold on; % Allow multiple plots on the same axes

% Define colors for visual distinction (optional)
colors = jet(num_triangles); 

for i = 1:num_triangles
    % Use 'patch' to plot a filled 2D polygon (the triangle)
    patch(X_coords(i, :), Y_coords(i, :), colors(i, :), ...
          'EdgeColor', 'k', 'LineWidth', 1.5, 'FaceAlpha', 0.6); 
    
    % Note: In 2D, the third argument to patch defines the color, 
    % not Z coordinates.
end

%% 4. Set Plot Properties
title('2D Triangles Plotted from Data File (x1, y1, x2, y2, x3, y3)');
xlabel('X-axis');
ylabel('Y-axis');
grid on;
axis equal; % Ensure equal scaling
hold off;