import Typography from "@mui/material/Typography";
import { Login, Logout } from "@mui/icons-material";
import Box from "@mui/material/Box";

import GenericButton from "../GenericButton";
import { ServiceProps } from "../types";
import { Grid } from "@mui/material";

export default function ServiceSetupModal() {
	const services: Array<ServiceProps> = [
		{
			imageSrc:
				"https://upload.wikimedia.org/wikipedia/fr/c/c8/Twitter_Bird.svg",
			altText: "Twitter",
		},
		{
			imageSrc:
				"https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
			altText: "Spotify",
		},
		{
			imageSrc:
				"https://upload.wikimedia.org/wikipedia/commons/d/d7/GitHub_font_awesome.svg",
			altText: "GitHub",
		},
		{
			imageSrc: "https://cdn.worldvectorlogo.com/logos/discord.svg",
			altText: "Discord",
		},
		{
			imageSrc:
				"https://upload.wikimedia.org/wikipedia/commons/3/39/Google_Mail.svg",
			altText: "Gmail",
		},
		{
			imageSrc:
				"https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
			altText: "YouTube",
		},
	];

	return (
		<div>
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
					justifyContent: "space-between",
					marginBottom: "25px",
				}}
			>
				<Typography variant="h4" noWrap align="left">
					Services
				</Typography>
			</Box>
			<Grid container spacing={4}>
				<Grid item xs container direction="column" spacing={2}>
					<Grid item>
						<Typography variant="h6" noWrap align="left">
							Available
						</Typography>
						<Box
							sx={{
								flexDirection: "column",
								alignItems: "center",
								justifyContent: "space-between",
								marginRight: "10px",
							}}
						>
							{services.map((elem, index) => (
								<Grid item mb={4} key={index}>
									<GenericButton
										service={elem}
										title={elem.altText}
										trailingIcon={<Login />}
									/>
								</Grid>
							))}
						</Box>
					</Grid>
				</Grid>
				<Grid item xs={12} sm container>
					<Grid item xs container direction="column" spacing={2}>
						<Grid item xs>
							<Typography variant="h6" noWrap align="right">
								Linked
							</Typography>
							<Box
								sx={{
									flexDirection: "column",
									alignItems: "center",
									justifyContent: "space-between",
									marginRight: "10px",
								}}
							>
								{services.map((elem, index) => (
									<Grid item mb={4} key={index}>
										<GenericButton
											service={elem}
											title={elem.altText}
											trailingIcon={<Logout />}
										/>
									</Grid>
								))}
							</Box>
						</Grid>
					</Grid>
				</Grid>
			</Grid>
		</div>
	);
}
