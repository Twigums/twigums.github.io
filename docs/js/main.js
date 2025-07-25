document.addEventListener("DOMContentLoaded", function () {

    // add ids here to let dark mode work
    const icon_map = {
        "theme-icon": {
            light: "/images/moon.svg",
            dark: "/images/sun.svg",
        },
        "github-icon": {
            light: "/images/github.svg",
            dark: "/images/github-white.svg",
        },
        "hakyll-icon": {
            light: "/images/hakyll.svg",
            dark: "/images/hakyll-white.svg",
        },
        "menu-icon": {
            light: "/images/menu.svg",
            dark: "/images/menu-white.svg",
        },
    };

    const theme = localStorage.getItem("theme");
    const is_dark = theme === "dark";

    function setIcons(is_dark) {
        for (const [id, paths] of Object.entries(icon_map)) {
            const img = document.getElementById(id);

            if (img) {
                img.src = is_dark ? paths.dark : paths.light;

            }
        }
    }

    function decodeEmail(encoded) {
        return atob(encoded);

    }

    window.toggleDarkMode = function () {
        const is_dark = document.documentElement.classList.toggle("dark-mode");
        localStorage.setItem("theme", is_dark ? "dark" : "light");
        setIcons(is_dark);

    };

    setIcons(is_dark);

    document.getElementById("hamburger-button").addEventListener("click", function () {
        const nav = document.getElementById("nav-menu");
        nav.classList.toggle("active");

    });

    document.getElementById("email").addEventListener("click", function() {
        const encoded = "dHdpZ3Vtcy5jb250YWN0QGdtYWlsLmNvbQ==";
        this.innerHTML = "<a href='mailto:" + atob(encoded) + "'>" + atob(encoded) + "</a>";

    });

});
