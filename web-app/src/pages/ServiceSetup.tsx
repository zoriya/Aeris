import Typography from "@mui/material/Typography";
import { Login, Logout } from "@mui/icons-material";
import Box from "@mui/material/Box";

import GenericButton from "../components/GenericButton";
import { ImageProps } from "../components/types";
import { Grid } from "@mui/material";
import { AppServices } from "../utils/globals";

export default function ServiceSetupModal() {
	return (
		<div>
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
					justifyContent: "space-between",
					marginBottom: "25px",
				}}>
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
							}}>
							{AppServices.map((elem, index) => (
								<Grid item mb={4} key={index}>
									<GenericButton service={elem.logo} title={elem.label} trailingIcon={<Login />} />
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
								}}>
								{AppServices.map((elem, index) => (
									<Grid item mb={4} key={index}>
										<GenericButton service={elem.logo} title={elem.label} trailingIcon={<Logout />} />
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
