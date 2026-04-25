interface Window {
    toggleDarkMode: () => void;
}

interface IconPaths {
    light: string;
    dark: string;
}

type IconMap = Record<string, IconPaths>;

document.addEventListener("DOMContentLoaded", function () {
    const iconMap: IconMap = {
        "theme-icon":  { light: "/images/moon.svg",         dark: "/images/sun.svg" },
        "github-icon": { light: "/images/github.svg",       dark: "/images/github-white.svg" },
        "hakyll-icon": { light: "/images/hakyll.svg",       dark: "/images/hakyll-white.svg" },
        "menu-icon":   { light: "/images/menu.svg",         dark: "/images/menu-white.svg" },
    };

    // inline <head> script already applied dark-mode class before paint
    let isDark = document.documentElement.classList.contains("dark-mode");

    function setIcons(dark: boolean): void {
        for (const [id, paths] of Object.entries(iconMap)) {
            const img = document.getElementById(id) as HTMLImageElement | null;
            if (img) {
                img.src = dark ? paths.dark : paths.light;
            }
        }
    }

    window.toggleDarkMode = function (): void {
        isDark = document.documentElement.classList.toggle("dark-mode");
        localStorage.setItem("theme", isDark ? "dark" : "light");
        setIcons(isDark);
    };

    setIcons(isDark);

    const hamburger = document.getElementById("hamburger-button");
    if (hamburger) {
        hamburger.addEventListener("click", function () {
            document.getElementById("nav-menu")?.classList.toggle("active");
        });
    }

    const emailEl = document.getElementById("email");
    if (emailEl) {
        emailEl.addEventListener("click", function () {
            const encoded = "dHdpZ3Vtcy5jb250YWN0QGdtYWlsLmNvbQ==";
            const decoded = atob(encoded);
            emailEl.innerHTML = `<a href='mailto:${decoded}'>${decoded}</a>`;
        });
    }
});
