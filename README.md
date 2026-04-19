# twigums.github.io

Source code for my website! The repo is generated using [Hakyll](https://jaspervdj.be/hakyll/), and the files are compiled to `./docs`. GitHub Actions will build and generate the site files directly to `docs` branch.

## How to build

During development, I used `stack` + `ghc` on Arch. So, ensure those are installed. I also followed [these instructions](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html) from the Hakyll tutorial to host the site on Github pages. However, the below steps are the tl;dr important steps for my future self!

1. Build `.hs` first:
   ```
   stack build
   ```
2. Build the website:
   ```
   stack exec site rebuild
   ```  
3. Verify the changes locally (skip this for pushing):
   ```
   stack exec site watch
   ```  
4. Add and commit all changes to git:
   ```
   git add -A
   git commit -m "{commit_message}"
   ```
5. Push to main branch (assuming you're there already!):
   ```
   git push origin
   ```

## Fun facts

- I combined color palettes from [IBM Color](https://www.ibm.com/design/language/color/) and [Catppuccin](https://github.com/catppuccin)!
- I learned you can edit `.svg` files without the use of applications, and [this online svg viewer](https://www.svgviewer.dev/) was very useful. I heard [Inkscape](https://inkscape.org/) is also good.

## Troubleshooting:

### GitHub auth token failure when using Actions

Change the setting of the repository to permit Actions' read/write. Navigate to: `Settings > Actions > General > Workflow permissions > Read and write permissions`. Enable `Read and write permissions`.
