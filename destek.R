
ghs_pop_yuzde <- function (yil1 = "1975", yil2 = "2020", cozunurluk = "100", ulke = "TUR", il = "Izmir")

{

  # İlk yıl ile ilgili işlemler


  data1 <- terra::rast(
    paste0("GHS_POP_E", yil1, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))

  crs <- terra::crs(data1)

  il_sinir <- geodata::gadm(country = ulke, level = 1, path = getwd()) |> sf::st_as_sf()

  il_sinir <- dplyr::filter(il_sinir, NAME_1 == il) |> terra::vect()

  il_sinir <- terra::project(il_sinir, crs)


  # İkinci yıl ile ilgili işlemler


  data2 <- terra::rast(
    paste0("GHS_POP_E", yil2, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))


  # İki yıla ait raster farklarının hesaplanıp sınıflandırılması

  fark <- data2 - data1

  degisim <- fark*100/data1

  yuzde <- terra::ifel(
    degisim < -400, -4,
    terra::ifel(degisim >= -400 & degisim < -200, -3,
                terra::ifel(degisim >= -200 & degisim < -100, -2,
                            terra::ifel(degisim >= -100 & degisim < -0.0001, -1,
                                        terra::ifel(degisim <= 100 & degisim > 0.0001, 1,
                                                    terra::ifel(degisim <= 200 & degisim > 101, 2,
                                                                terra::ifel(degisim <= 400 & degisim > 201, 3,
                                                                            terra::ifel(degisim > 400, 4, degisim)
                                                                )
                                                    )
                                        )
                            )
                )
    )
  )

  value <- data.frame(c(-4, -3, -2, -1, 1, 2, 3, 4),
                         c("-400'den az", "-401 - -200", "-201 - -100", "-101 - 0", "0 - 100", "101 - 200", "201 - 400", "401'den fazla"))

  levels(yuzde) <- value

  ggplot2::ggplot() +
    tidyterra::geom_spatraster( data = yuzde) +
    ggplot2::geom_sf(data = il_sinir,
                     fill = "transparent",
                     color = "grey40",
                     size = .5) +
    ggplot2::scale_fill_manual(
      name = "Artış (%) / Azalış (%)",
      values = c("#a50026","#d73027","#f46d43","#fdae61","#abd9e9","#74add1","#4575b4","#313695"),
      labels = c(
        "% -400",
        "-401 - -200",
        "-201 - -100",
        "-101 - 0",
        "0 - 100",
        "101 - 200",
        "201 - 400",
        "401'den fazla"
      ),
      na.translate = FALSE
    ) +
    ggplot2::ggtitle(
      paste0( yil1, " - ", yil2, " Yılları Arası İzmir İli Nüfus Değişimi")
    )
}
