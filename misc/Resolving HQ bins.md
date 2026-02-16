# Resolving HQ bins

### Differences between Jackson's bins & Nadav's:

1. **HQ<1**. Nadav removes these. Jackson sets these to 0.
   1. If 95th percentile is being selected from the actual data, Jackson's will lead to lower values (more data to the left of the distribution means a larger 5% taken), which is more accurate to what's there. Nadav's pretends these "good values" don't exist.
   2. Recommendation: Don't change HQ<1 now that we're not summing, since the data won't show when we select 95th%ile. Will be most similar to Jackson's method.
2. **Date range selected.** Nadav selects 5 most recent years for each station. Jackson selects 5 years (based on most recent date from the entire dataset). 
   1. Differences: Canutillos 3 was only sampled in 2017 and is only captured using Nadav's method. Jackson has more robust date processing and captures a true 5 year range. Most locations were sampled in 2024 so it doesn't matter too much across the board.
   2. Recommendation: To keep Canutillos 3 data, use Nadav's method and Jackson's date processing (range = [max_date - 365.25*5, max_date])
   3. Image: "last_5_years" is Jackson's method (2019-2024), "5_station_years" is Nadav's method (year > max_year - 5). 
   4. ![image-20260214121146628](C:\Users\nadav\AppData\Roaming\Typora\typora-user-images\image-20260214121146628.png)