/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./web-ui/view/**/*.ml"
  ],
  theme: {
    extend: {
      colors: {
        warning: {
          50: "#FFFAEB",
          500: "#F79009",
        }
      }
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    // ...
  ],
}
