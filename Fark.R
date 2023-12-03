
deneme <- function (yil1 = "1975", yil2 = "2020", cozunurluk = "100", ulke = "TUR", il = "Izmir")

{

  # İlk yıl ile ilgili işlemler


  data1 <- terra::rast(
    paste0("GHS_POP_E", yil1, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))



  # İkinci yıl ile ilgili işlemler


  data2 <- terra::rast(
    paste0("GHS_POP_E", yil2, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))


  # İki yıla ait raster farklarının hesaplanıp sınıflandırılması

  fark <- data2 - data1

  fark <- terra::ifel(
    fark >= 1000, 11, terra::ifel(
      fark >= 900 & fark < 1000, 10, terra::ifel(
        fark >= 800 & fark < 900, 9, terra::ifel(
          fark >= 700 & fark < 800, 8, terra::ifel(
            fark > 600 & fark < 700, 7, terra::ifel(
              fark >= 500 & fark < 600, 6, terra::ifel(
                fark >= 400 & fark < 500, 5, terra::ifel(
                  fark >= 300 & fark < 400, 4, terra::ifel(
                    fark >= 200 & fark < 300, 3, terra::ifel(
                      fark >= 100 & fark < 200, 2, terra::ifel(
                        fark > 0 & fark < 100, 1, terra::ifel(
                          fark == 0, 0, terra::ifel(
                            fark >= -100 & fark < 0, -1, terra::ifel(
                              fark >= -200 & fark < -300, -2, terra::ifel(
                                fark >= -300 & fark < -400, -3, terra::ifel(
                                  fark >= -400, -4,  fark
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  value <- data.frame(a = c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4),
                          b = c(
                            "1000 ve daha yüksek",
                            "900 - 1000",
                            "800 - 900",
                            "700 - 800",
                            "600 - 700",
                            "500 - 600",
                            "400 - 500",
                            "300 - 400",
                            "200 - 300",
                            "100 - 200",
                            "0 - 100",
                            "0",
                            "-100 - 0",
                            "-200 - -100",
                            "-200 - -300",
                            "-300 ve daha düşük"
                            ))

  levels(fark) <- value

  ggplot2::ggplot() +
    tidyterra::geom_spatraster( data = fark) +
    ggplot2::scale_fill_manual(
      values = c("1000 ve daha yüksek" = "#49006a",
                 "900 - 1000" = "#7a0177",
                 "800 - 900" = "#ae017e",
                 "700 - 800" = "#dd3497",
                 "600 - 700" = "#08306b",
                 "500 - 600" = "#08519c",
                 "400 - 500" = "#2171b5",
                 "300 - 400" = "#4292c6",
                 "200 - 300" = "#6baed6",
                 "100 - 200" = "#9ecae1",
                 "0 - 100" = "#c6dbef",
                 "0" = "white",
                 "-100 - 0" = "#fee5d9",
                 "-200 - -100" = "#fcae91",
                 "-200 - -300" = "#fb6a4a",
                 "-300 ve daha düşük" = "#cb181d")
    ) +
    ggplot2::coord_sf(crs = "EPSG:4326") +
    ggplot2::ggtitle(paste0(yil1, "-", yil2, " Yillari Arasi Izmir Ili Nufus Dagilimi Degisimi"))

}
