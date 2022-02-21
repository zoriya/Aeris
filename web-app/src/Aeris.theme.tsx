import { createTheme } from "@mui/material/styles";

const aerisTheme = createTheme({
	palette: {
		primary: {
			light: "#62717b",
			main: "#37474F",
			dark: "#101f27",
			contrastText: "#ffffff",
		},
		secondary: {
			light: "#ff94c2",
			main: "#F06292",
			dark: "#ba2d65",
			contrastText: "#000000",
		},
	},
});

export default aerisTheme;
