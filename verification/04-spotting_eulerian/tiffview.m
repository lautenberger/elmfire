clear
img = imread('outputs/time_of_arrival_0000001_0001002.tif');
image(img,'CDataMapping','scaled')
axis equal
caxis([0 1000])