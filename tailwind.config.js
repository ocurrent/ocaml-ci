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
        },
        gray: {
            850: "#1B263D",
        }
      },
      typography (theme) {
        return {
          DEFAULT: {
            css: {
              'code::before': {
                content: 'none', // don’t generate the pseudo-element
//                content: '""', // this is an alternative: generate pseudo element using an empty string
              },
              'code::after': {
                content: 'none'
              },
              code: {
                color: theme('colors.slate.500'),
                backgroundColor: theme('colors.stone.100'),
                borderRadius: theme('borderRadius.DEFAULT'),
                paddingLeft: theme('spacing[1.5]'),
                paddingRight: theme('spacing[1.5]'),
                paddingTop: theme('spacing.1'),
                paddingBottom: theme('spacing.1'),
              },
            }
          }
        }
      }
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    // ...
  ],
}
