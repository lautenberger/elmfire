clear
img = imread('outputs/time_of_arrival_0000001_0010002.tif');
img = sum(img, 3);
image(img,'CDataMapping','scaled')
axis equal
caxis([0 4000])