interface Window {
    toggleDarkMode: () => void;
}

document.addEventListener("DOMContentLoaded", function () {
    window.toggleDarkMode = function (): void {
        const toggle = () => {
            const isDark = document.documentElement.classList.toggle("dark-mode");
            localStorage.setItem("theme", isDark ? "dark" : "light");
        };

        if ("startViewTransition" in document) {
            (document as unknown as { startViewTransition: (cb: () => void) => void })
                .startViewTransition(toggle);
        } else {
            toggle();
        }
    };

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
