#!/bin/bash
set -eu

if [ -f rayhunter ]; then
  RAYHUNTER=./rayhunter
else
  RAYHUNTER=rayhunter
fi

$RAYHUNTER classic 3 800 600 ../demo-models/gltf/punctual_lights/test_lights.gltf \
  test_output_gltf_test_lights.png
$RAYHUNTER classic 3 1024 768 ../demo-models/lights_materials/lights.x3dv \
  test_output_lights.png
$RAYHUNTER classic 3 1024 768 ../demo-models/lights_materials/raytracer/alien_mirror.wrl \
  test_output_alien_mirror.png
$RAYHUNTER path 3 10 512 512 ../demo-models/lights_materials/raytracer/bowl_of_soup_final.x3dv \
  test_output_bowl_of_soup.png
$RAYHUNTER path 3 10 512 512 ../demo-models/lights_materials/raytracer/area_light_test_final.wrl \
  test_output_area_light_test_final.png
