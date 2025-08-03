function Image (img)
  img.src = img.src:gsub("^/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/(.*)/(.*).png", "/Series%20Temporales/output/\\1/\\2.png")
  return img
end