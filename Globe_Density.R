library(rayshader)
library(rayrender)
library(raster)
library(magick)

popdata = raster("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min.tif")

population_mat = raster_to_matrix(popdata)
population_mat=population_mat[nrow(population_mat):1,]

above1 = population_mat > 1
above5 = population_mat > 5
above10 = population_mat > 10
above50 = population_mat > 50
above100 = population_mat > 100
above500 = population_mat > 500
above1000 = population_mat > 1000

above1[is.na(above1)] = 0
above5[is.na(above5)] = 0
above10[is.na(above10)] = 0
above50[is.na(above50)] = 0
above100[is.na(above100)] = 0
above500[is.na(above500)] = 0
above1000[is.na(above1000)] = 0


turbocols = viridis::turbo(7)
wc = 0.4

chart_items = 
  xy_rect(x=-1,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
          material=diffuse(color="grey30")) %>% 
  add_object(text3d(label = "0", x=-1,y=-1.4,z=1.01, text_height = 0.1, 
                    material=diffuse(color="black"))) %>% 
  add_object(xy_rect(x=-0.6,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
                     material=diffuse(color=turbocols[1]))) %>% 
  add_object(text3d(label = "1>", x=-0.6,y=-1.4,z=1.01, text_height = 0.1, 
                    material=diffuse(color="black"))) %>% 
  add_object(xy_rect(x=-0.2,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
                     material=diffuse(color=turbocols[2]))) %>% 
  add_object(text3d(label = "5>", x=-0.2,y=-1.4,z=1.01, text_height = 0.1, 
                    material=diffuse(color="black"))) %>% 
  add_object(xy_rect(x=0.2,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
                     material=diffuse(color=turbocols[3]))) %>% 
  add_object(text3d(label = "10>", x=0.2,y=-1.4,z=1.01, text_height = 0.1, 
                    material=diffuse(color="black"))) %>% 
  add_object(xy_rect(x=0.6,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
                     material=diffuse(color=turbocols[4]))) %>% 
  add_object(text3d(label = "50>", x=0.6,y=-1.4,z=1.01, text_height = 0.1, 
                    material=diffuse(color="black"))) %>% 
  add_object(xy_rect(x=1.0,y=-1.4,z=1,xwidth=wc,ywidth=0.2, 
                     material=diffuse(color=turbocols[5]))) %>% 
  add_object(text3d(label = "100>", x=1.0,y=-1.4,z=1.01, text_height = 0.1,
                    material=diffuse(color="black"))) %>%
  add_object(xy_rect(x=1.4,y=-1.4,z=1,xwidth=wc,ywidth=0.2,
                     material=diffuse(color=turbocols[6]))) %>%
  add_object(text3d(label = "500>", x=1.4,y=-1.4,z=1.01, text_height = 0.1,
                    material=diffuse(color="black"))) %>%
  add_object(xy_rect(x=1.8,y=-1.4,z=1,xwidth=wc,ywidth=0.2,
                     material=diffuse(color=turbocols[7]))) %>%
  add_object(text3d(label = "1000>", x=1.8,y=-1.4,z=1.01, text_height = 0.1,
                    material=diffuse(color="black"))) %>%
  add_object(text3d(label = "People per 30km^2", x=-0.55,y=-1.2,z=1.01, text_height = 0.15,
                    material=diffuse(color="white"))) %>%
  group_objects(translate = c(-0.4,0,0),scale=c(0.85,0.85,0.85))

radm = 1.2
for(i in 1:720) {
  chart_items %>% 
    add_object(group_objects(
      sphere(radius=0.99*radm,material=diffuse(color="grey20")) %>% 
        add_object(sphere(radius=1.0*radm,material= diffuse(color=turbocols[1],alpha_texture = above1))) %>% 
        add_object(sphere(radius=1.02*radm,material=diffuse(color=turbocols[2],alpha_texture = above5))) %>% 
        add_object(sphere(radius=1.03*radm,material=diffuse(color=turbocols[3],alpha_texture = above10))) %>% 
        add_object(sphere(radius=1.04*radm,material=diffuse(color=turbocols[4],alpha_texture = above50))) %>% 
        add_object(sphere(radius=1.05*radm,material=diffuse(color=turbocols[5],alpha_texture = above100))) %>% 
        add_object(sphere(radius=1.06*radm,material=diffuse(color=turbocols[6],alpha_texture = above500))) %>% 
        add_object(sphere(radius=1.07*radm,material=diffuse(color=turbocols[7],alpha_texture = above1000))),
      angle = c(0,-i/2,0))) %>% 
    add_object(sphere(y=10,z=5,radius=3,material=light(intensity = 20))) %>%
    add_object(sphere(y=0,z=20,radius=3,material=light(intensity = 20))) %>%
    render_scene(width=1000,height=1000,samples=128,rotate_env = 180,clamp_value = 10,
                 aperture=0,
                 filename=sprintf("worldpopfocus%i.png",i), lookat=c(0,-0.2,0))
}