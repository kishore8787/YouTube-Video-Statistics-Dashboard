# 🎬 YouTube Video Statistics Dashboard (Shiny App)

An interactive **R Shiny application** that allows you to **search YouTube videos**, select multiple ones, and **compare their statistics** (views, likes, and comments) with beautiful visualizations.  

---

## 🚀 Features
- 🔍 **Search YouTube videos** by title using the YouTube Data API v3  
- 🎥 **Preview video thumbnails** and titles before selecting  
- ➕ **Add/Remove videos** dynamically for comparison  
- 📊 **Compare multiple videos** on:
  - Views  
  - Likes  
  - Comments  
- 🌙 **Modern UI with dark theme** & custom CSS styling  

---

## 🛠️ Tech Stack
- **R Shiny** (for UI & interactivity)  
- **ggplot2** (for data visualization)  
- **httr + jsonlite** (for API requests)  
- **shinythemes** (UI styling)  

---

## 🔑 Setup Instructions

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-username/your-repo-name.git
   cd your-repo-name
   ```

2. **Install required R packages**
   ```R
   install.packages(c("shiny", "shinythemes", "httr", "jsonlite", "ggplot2", "reshape2", "scales"))
   ```

3. **Get a YouTube API key**
   - Go to [Google Cloud Console](https://console.cloud.google.com/)  
   - Enable **YouTube Data API v3**  
   - Create an API key  

4. **Set your API key in R**
   ```R
   Sys.setenv(MY_YT_API_KEY = "your_api_key_here")
   ```

5. **Run the app**
   ```R
   shiny::runApp("app.R")
   ```

---

## 📂 Project Structure
```
├── app.R        # Main Shiny App code
├── README.md    # Project Documentation
```

---

## 🙌 Acknowledgements
- [YouTube Data API v3](https://developers.google.com/youtube/v3)  
- [Shiny R Framework](https://shiny.posit.co/)  

---

## ⭐ Future Improvements
- Add **subscriber count** comparison  
- Export comparison results as **CSV/PDF**  
- Add support for **real-time video analytics**  

---

👨‍💻 Developed with ❤️ using **R Shiny**
