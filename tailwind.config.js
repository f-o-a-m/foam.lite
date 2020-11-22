const defaultColors = require('tailwindcss/defaultTheme').colors;

module.exports = {
  important: true,
  future: {
    // removeDeprecatedGapUtilities: true,
    // purgeLayersByDefault: true,
  },
  purge: [],
  theme: {
    fontFamily: {
      sans: ['Inter', 'system-ui', '-apple-system', 'BlinkMacSystemFont', 'Segoe UI', 'Roboto', 'Helvetica Neue', 'Arial', "Noto Sans", 'sans-serif', 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'],
    },
    // curating colors instead of `extend`ing them substantially reduces final CSS size
    colors: {
      navbar_dark: "#141515",
      text_lightgray: "#D8DCDE",
      dullergray: "#ACAFB0",
      bg_gradient_light: "#2B2B2B",
      bg_gradient_dark: "#000000",
      transparent: defaultColors.transparent,
      current: defaultColors.current,
      white: defaultColors.white,
      black: defaultColors.black,
    },
    extend: {
      screens: {
        'xxl': '2047px',
        '3xl': '2559px',
        '4xl': '3199px',
      },
      maxWidth: {
        '1/5': '20%',
        '1/4': '25%',
        '1/3': '33.333333%',
        '2/5': '20%',
        '1/2': '50%',
        '2/4': '50%',
        '3/5': '60%',
        '2/3': '66.666666%',
        '3/4': '75%',
        '4/5': '80%',
        '90p': '90%',
        'screen-sans-scrollbar': 'calc(100vw - (100vw - 100%))',
      },
      minHeight: {
        'card': '10rem',
        'screen-sans-scrollbar': 'calc(100vh - (100vh - 100%))',
        'screen-sans-3-rem': 'calc(100vh - 3rem)',
      },
      maxHeight: {
        'screen-sans-scrollbar': 'calc(100vh - (100vh - 100%))',
      },
      width: {
        '90p': '90%',
        'screen-sans-scrollbar': 'calc(100vw - (100vw - 100%))',
      },
      height: {
        'card': '10rem',
        'screen-sans-scrollbar': 'calc(100vh - (100vh - 100%))',
        'screen-sans-3-rem': 'calc(100vh - 3rem)',
      },
      transitionProperty: {
        'all-positions': 'margin, padding, top, bottom, left, right'
      },
      "padding": {
        "4.5": "1.125rem"
      }
    },
  },
  variants: {
    borderWidth: ({ after }) => after(['last']),
  },
  plugins: [],
}
