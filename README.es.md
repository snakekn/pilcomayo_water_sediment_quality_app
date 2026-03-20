## Acerca del Proyecto

La aplicación Explorador de Calidad de Agua y Sedimento permite a los usuarios cargar datos de calidad de agua y sedimentos para generar visualizaciones dinámicas y mapas de la Cuenca del Pilcomayo.

Puede encontrar información adicional sobre este proyecto en el sitio web del Programa de Maestría de la Escuela Bren de Medio Ambiente.

La aplicación está alojada en Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19104674.svg)](https://doi.org/10.5281/zenodo.19104674)


También está disponible en línea en [https://github.com/snakekn/pilcomayo_water_sediment_quality_app](https://github.com/snakekn/pilcomayo_water_sediment_quality_app).

## Acerca de la Aplicación
### Datos Disponibles

La aplicación utiliza datos de la Comisión Trinacional de los años 2016 a 2023, con la opción de incluir datos de 2024 (ver sección 4.1 Limitaciones de los Datos).

Los usuarios pueden filtrar los datos por analito, estación de muestreo y rangos de tiempo, y visualizar los resultados según valores brutos, comparaciones con estándares o factores de riesgo como el cociente de peligro (HQ, por sus siglas en inglés).

Es posible cargar nuevos datos ambientales muestreados en la región utilizando una guía de formato proporcionada o el formato empleado por la Comisión Trinacional, respaldando el análisis de futuros monitoreos ambientales realizados por AMTSK, organizaciones comunitarias locales, agencias gubernamentales y otros actores interesados.

### Gráficos de Clasificación

Una serie de gráficos clasifica las concentraciones reportadas en agua o sedimento mediante:
- Clasificación de muestras individuales según concentración o HQ.
- Clasificación de estaciones según HQ.
- Clasificación de parámetros peligrosos.
- Clasificación de fracciones granulométricas según HQ.

Cada gráfico puede filtrarse para centrarse en parámetros, medios, fracciones o estaciones específicas.
Los parámetros de sedimento se agruparon por tamaño de malla (fracción granulométrica) utilizando el sistema de clasificación de tamaños de grano de Wentworth (Wentworth, 1922). 

### Gráficos de Series Temporales

Se generaron gráficos de series temporales para evaluar tendencias en la calidad de agua y sedimento a lo largo del tiempo.
El usuario puede seleccionar un parámetro y una ubicación para generar un gráfico que muestre cómo han variado los valores medidos de ese parámetro con el tiempo. También puede mostrar límites de referencia relevantes para comparar con los valores observados.

### Mapas Combinados de Riesgo

Se pueden generar capas que combinen los peligros derivados de la calidad del agua y los sedimentos con los índices municipales de EJI (Índice de Justicia Ambiental) y la densidad poblacional.
Las capas de riesgo de agua y sedimento se crean asignando puntajes a las estaciones (ver sección 3.3.1 Agregación del Cociente de Peligro) y interpolando esos puntajes a través de la red fluvial (ver sección 3.3.1 Interpolación a lo largo de la Red de Ríos).
Los usuarios pueden ajustar el método de puntuación de las estaciones. Los puntajes de EJI y la densidad poblacional están integrados en la aplicación.
Una vez generadas las capas y seleccionados los métodos de clasificación de puntajes (por ejemplo, Intervalo Igual, Cuantiles), pueden integrarse en un mapa final de priorización para asignar niveles de prioridad a las zonas más impactadas de la cuenca.
También pueden mostrarse las ubicaciones de las minas, asentamientos, ríos, relaves, plantas de beneficio y subcuencas.
Las subcuencas pueden delimitarse utilizando las estaciones de agua y sedimento como puntos de salida, y se les asignan los puntajes correspondientes a sus estaciones asociadas.
