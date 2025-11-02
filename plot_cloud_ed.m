% --- MATLAB Code Snippet to Read and Plot Data ---

% The data is assumed to be in a file named 'data.txt' in the current folder.
% The content of 'data.txt' is expected to be:
%  1.941288552764E+00  3.069713605584E-01
%  2.114464402611E+00  2.653939114146E-01
%  1.787211723751E+00  3.381952052478E-01
%  1.809243882296E+00  5.567297080463E-01
%  1.674194968132E+00  7.210549125343E-01

% 1. Define the filename
filename = 'xy.txt';

% 2. Read the data from the file into a matrix
% 'readmatrix' is the recommended function for reading numeric data from text files.
try
    data = readmatrix(filename);
catch ME
    fprintf('Error reading file: %s\n', ME.message);
    disp('Please ensure the file "data.txt" exists and contains only numeric data.');
    return; % Exit if reading fails
end

% 3. Separate the data into X and Y vectors
X = data(:, 1); % First column is X
Y = data(:, 2); % Second column is Y
fileName = 'edges.txt'; % Replace with your actual file name
data = dlmread(fileName); % Reads the file into a matrix
x_start = data(:, 1)'; % All rows, 1st column
y_start = data(:, 2)'; % All rows, 2nd column
x_end = data(:, 3)';   % All rows, 3rd column
y_end = data(:, 4)';   % All rows, 4th column
% 4. Plot the points
figure; % Create a new figure window
plot([x_start;x_end], [y_start; y_end], '--b', 'LineWidth', .5);
hold on;
scatter(X, Y, 5, 'black', 'filled');
title('Plot of Points from File');
xlabel('X Data');
ylabel('Y Data');
grid on;
hold off;
disp('Data read successfully and plotted.');
% disp('X values:');
% disp(X');
% disp('Y values:');
% disp(Y');