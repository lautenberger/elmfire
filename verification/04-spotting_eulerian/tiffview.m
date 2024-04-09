clear
img = imread('outputs/hrr_transient_0000001_0003606.tif');
image(img,'CDataMapping','scaled')
axis equal
caxis([0 10])