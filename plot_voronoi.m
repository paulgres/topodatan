% --- MATLAB Code Snippet to Read and Plot Data with Indexed Colors ---

% The data is assumed to be in a file named 'data.txt' in the current folder,
% with three columns: X, Y, and a color index (1 to 24).

% 1. Define the filename
filename = 'xy.txt';

% 2. Read the data from the file into a matrix
try
    data = readmatrix(filename);
catch ME
    fprintf('Error reading file: %s\n', ME.message);
    disp('Please ensure the file "data.txt" exists and contains three columns of numeric data.');
    return; % Exit if reading fails
end

% 3. Separate the data into X, Y, and Color Index (C) vectors
X = data(:, 1); % First column is X coordinate
Y = data(:, 2); % Second column is Y coordinate
C = data(:, 3); % Third column is the color index (1 to 24)

% 4. Set up the plotting environment
figure; % Create a new figure window

% 5. Create a colormap with 24 distinct colors
% The 'jet' colormap is a good default for generating a wide range of colors.
% You can change 'jet' to 'hsv', 'parula', or another colormap if preferred.
colormap(jet(24)); 

% 6. Plot the data using the 'scatter' function
% The 'scatter' function plots (X, Y) points and uses the vector C to determine the color.
% The MarkerSize is set to a reasonable value (e.g., 50) for visibility.
h = scatter(X, Y, 5, C, 'filled', 'MarkerEdgeColor', 'k');

% 7. Set the color axis limits
% This ensures that the indices 1 through 24 map exactly to the 24 colors in the colormap.
caxis([1 24]);

% 8. Add a colorbar and labels
cb = colorbar;
title('Data Points with Color-Coded Index');
xlabel('X Coordinate');
ylabel('Y Coordinate');
ylabel(cb, 'Color Index (1 to 24)', 'FontSize', 10);
grid on;

disp('Data read successfully and plotted using colors based on the 3rd column index.');