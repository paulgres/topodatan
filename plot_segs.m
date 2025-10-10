% --- 1. Import the data ---
fileName = 'edges.txt'; % Replace with your actual file name
data = dlmread(fileName); % Reads the file into a matrix

% Alternatively, use readmatrix (available in newer MATLAB versions):
% data = readmatrix(fileName);

% The data matrix now has 4 columns:
% Column 1: x_start
% Column 2: y_start
% Column 3: x_end
% Column 4: y_end

x_start = data(:, 1); % All rows, 1st column
y_start = data(:, 2); % All rows, 2nd column
x_end = data(:, 3);   % All rows, 3rd column
y_end = data(:, 4);   % All rows, 4th column

% --- 2. Prepare data for plotting line segments ---
% To plot N segments, you need an X-vector and a Y-vector, each with 2*N + 1 elements.
% Each segment is defined by [x_start, x_end, NaN] and [y_start, y_end, NaN].
% The 'NaN' is crucial; it breaks the line between segments so they aren't connected.

% Reshape the start and end points into a structure suitable for the plot function:
% X-coordinates: [x1_start, x1_end, NaN, x2_start, x2_end, NaN, ...]
X_coords = [x_start, x_end, nan(size(x_start))];
X_plot = X_coords'; % Transpose to get a single column vector
X_plot = X_plot(:); % Convert to a single column vector

% Y-coordinates: [y1_start, y1_end, NaN, y2_start, y2_end, NaN, ...]
Y_coords = [y_start, y_end, nan(size(y_start))];
Y_plot = Y_coords'; % Transpose to get a single column vector
Y_plot = Y_plot(:); % Convert to a single column vector

% --- 3. Plot the line segments ---
figure; % Create a new figure window
plot(X_plot, Y_plot, '--b', 'LineWidth', .5); % Plot as blue lines

% --- 4. Add labels and title (optional but recommended) ---
xlabel('X Coordinate');
ylabel('Y Coordinate');
title('Line Segments Imported from File');
grid on;
axis equal; % Ensure proper aspect ratio for visualization