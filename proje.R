
######################################################################
#            RASTER VERİLERİ İNDİRME FONKİSYONU                      #
######################################################################


ghs_indir <- function(yil1 = "1975", cozunurluk = "100") {
  url1 <- paste0("jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",
                 yil1,
                 "_GLOBE_R2023A_54009_",
                 cozunurluk,
                 "/V1-0/tiles/GHS_POP_E",
                 yil1,
                 "_GLOBE_R2023A_54009_",
                 cozunurluk,
                 "_V1_0_R5_C21.zip"
  )

  httr::GET(
    url1,
    httr::write_disk("dosya_ismi.zip", overwrite = T),
    httr::progress()
  )

  unzip("dosya_ismi.zip")

  file.remove(c("dosya_ismi.zip",
                "GHSL_Data_Package_2023_light.pdf",
                "GHS_POP_GLOBE_R2023A_input_metadata.xlsx"))
}

######################################################################
#                        KIRPMA FONKSİYONU                           #
######################################################################


kirp <- function(yil = "1975") {

  spatraster <-paste0("GHS_POP_E", yil, "_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif")

  data <- terra::rast(spatraster)

  crs <- terra::crs(data)

  il_sinir <- terra::project(il_sinir, crs)

  data <- terra::crop(data, il_sinir, mask = T )

  terra::writeRaster(data, spatraster, overwrite = T)

}


######################################################################
#    SONUÇLARIN HESAPLANMASI VE HARİTALANMASI FONKSİYONU             #
######################################################################



ghs_pop_artma_azalma <- function (yil1 = "1975", yil2 = "2020", cozunurluk = "100", ulke = "TUR", il = "Izmir")

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

  degisim <- terra::ifel(fark < 0, -1,
                         terra::ifel(fark == 0, 0,
                                     terra::ifel(fark > 0 , 1, fark)))

  value <- data.frame(c(-1, 0, 1), c("Azalış", "Yerleşim Yok", "Artış"))

  levels(degisim) <- value

  # Haritalamanın gerçekleştirilmesi
  cols <- c(
    "#eb389f",
    "grey80",
    "#018f1d"
  )

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster( data = degisim) +
    ggplot2::geom_sf(data = il_sinir,
                     fill = "transparent",
                     color = "grey40",
                     size = .5) +
    ggplot2::scale_fill_manual(
      name = "Artış / Azalış",
      values = cols,
      labels = c(
        "Azalış",
        "Yeleşim Yok",
        "Artış"
      ),
      na.translate = FALSE
    ) +
    ggplot2::ggtitle(
      paste0( yil1, " - ", yil2, " Yılları Arası İzmir İli Nüfus Değişimi")
    )

  # sonucun kaydedilmesi

  w <- ncol(degisim)
  h <- nrow(degisim)

  ggplot2::ggsave(
    filename = paste0(yil1, "-", yil2, "_fark.tiff"),
    p,
    width = w,
    height = h,
    units = "px",
    bg = "white"
  )
}

################################################################################
#       YILLAR ARASI FARKLARIN HESAPLANMASI VE HARİTALANMASI FONKSİYONU        #
################################################################################


ghs_pop_fark <- function (yil1 = "1975", yil2 = "2020", cozunurluk = "100", ulke = "TUR", il = "Izmir")

{

  # İlk yıl ile ilgili işlemler


  data1 <- terra::rast(
    paste0("GHS_POP_E", yil1, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))



  # İkinci yıl ile ilgili işlemler


  data2 <- terra::rast(
    paste0("GHS_POP_E", yil2, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))



  # il ve ilçe sınırlarını indirme

  il_sinir <- geodata::gadm(country = ulke, level = 2, path = getwd())
  il_sinir <- dplyr::filter(il_sinir, NAME_1 == il)

  # İki yıla ait raster farklarının hesaplanıp sınıflandırılması

  fark <- data2 - data1

  fark[fark >= 300] <- 8
  fark[fark >= 200 & fark < 300] <- 7
  fark[fark >= 150 & fark < 200] <- 6
  fark[fark >= 100 & fark < 150] <- 5
  fark[fark >= 75 & fark < 100] <- 4
  fark[fark >= 50 & fark < 75] <- 3
  fark[fark >= 25 & fark < 50] <- 2
  fark[fark > 0 & fark < 25] <- 1
  fark[fark < 0 & fark > -25] <- -1
  fark[fark <= -25 & fark > -50] <- -2
  fark[fark <= -50 & fark > -75] <- -3
  fark[fark <= -75 & fark > -100] <- -4
  fark[fark <= -100 & fark > -150] <- -5
  fark[fark <= -150 & fark > -200] <- -6
  fark[fark <= -200 & fark > -300] <- -7
  fark[fark <= -300] <- -8

  value <- data.frame(a = c(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8),
                      b = c(
                        "300 ve uzeri",
                        "200 - 300",
                        "150 - 200",
                        "100 - 150",
                        "75 - 100",
                        "50 - 75",
                        "25 - 50",
                        "0 - 25",
                        "0",
                        "0 - 25",
                        "-25 - -50",
                        "-50 - -75",
                        "-75 - -100",
                        "-100 - -150",
                        "-150 - -200",
                        "-200 - - 300",
                        "-300 ve altı"
                      ))

  levels(fark) <- value

  ggplot2::ggplot() +
    tidyterra::geom_spatraster( data = fark, interpolate = F) +
    ggplot2::scale_fill_manual(
      values = c(
        "300 ve uzeri" = "#49006a",
        "200 - 300" = "#7a0177",
        "150 - 200" = "#ae017e",
        "100 - 150" = "#dd3497",
        "75 - 100" = "#08519c",
        "50 - 75" = "#2171b5",
        "25 - 50" = "#4292c6",
        "0 - 25" = "#6baed6",
        "0" = "white",
        "0 - 25" = "#fcbba1",
        "-25 - -50" = "#fc9272",
        "-50 - -75" = "#fb6a4a",
        "-75 - -100" = "#ef3b2c",
        "-100 - -150" = "#cb181d",
        "-150 - -200" =  "#a50f15",
        "-200 - - 300" = "#67000d",
        "-300 ve altı" = "#993404"
      )
    ) +
    ggplot2::geom_sf(data = il_sinir, fill = "transparent", color = "gray30", size = .5) +
    ggplot2::coord_sf(crs = "EPSG:4326") +
    ggplot2::ggtitle(paste0(yil1, "-", yil2, " Yillari Arasi Izmir Ili Nufus Dagilimi Degisimi"))

}


################################################################################
#      SONUÇLARIN YÜZDE OLARAK HESAPLANMASI VE HARİTALANMASI FONKSİYONU        #
################################################################################



ghs_pop_yuzde <- function (yil1 = "1975", yil2 = "2020", cozunurluk = "100", ulke = "TUR", il = "Izmir")

{

  # İlk yıl ile ilgili işlemler


  data1 <- terra::rast(
    paste0("GHS_POP_E", yil1, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))

  crs <- terra::crs(data1)

  il_sinir <- geodata::gadm(country = ulke, level = 2, path = getwd()) |> sf::st_as_sf()

  il_sinir <- dplyr::filter(il_sinir, NAME_1 == il) |> terra::vect()

  il_sinir <- terra::project(il_sinir, crs)


  # İkinci yıl ile ilgili işlemler


  data2 <- terra::rast(
    paste0("GHS_POP_E", yil2, "_GLOBE_R2023A_54009_", cozunurluk, "_V1_0_R5_C21.tif"))


  # İki yıla ait raster farklarının hesaplanıp sınıflandırılması

  fark <- data2 - data1

  degisim <- fark*100/data2

  yuzde <-



  # Haritalamanın gerçekleştirilmesi

  ggplot2::ggplot() +
    tidyterra::geom_spatraster( data = degisim) +
    ggplot2::geom_sf(data = il_sinir,
                     fill = "transparent",
                     color = "grey40",
                     size = .5) +
    ggplot2::scale_fill_stepsn(colours = terrain.colors(10)
    ) +
    ggplot2::ggtitle(
      paste0( yil1, " - ", yil2, " Yılları Arası İzmir İli Nüfus Değişimi (%)")
    )

  # sonucun kaydedilmesi

  '''w <- ncol(degisim)
  h <- nrow(degisim)

  ggplot2::ggsave(
    filename = paste0(yil1, "-", yil2, "_Yuzde.tiff"),
    p,
    width = w,
    height = h,
    units = "px",
    bg = "white"
  )'''
}


