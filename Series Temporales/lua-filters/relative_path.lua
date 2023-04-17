function Image (img)
  img.src = img.src:gsub('^/.*/Series Temporales/output/Lab-Session-8-FPP_files/(.*).png', '/Series Temporales/output/Lab-Session-8-FPP_files/\\1.png')
  return img
end