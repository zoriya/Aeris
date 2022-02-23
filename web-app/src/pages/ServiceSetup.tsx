import Typography from "@mui/material/Typography";
import { Login, Logout } from "@mui/icons-material";
import Box from "@mui/material/Box";

import GenericButton from "../components/GenericButton";
import { ImageProps } from "../components/types";
import { Grid } from "@mui/material";
import { AppServices } from "../utils/globals";
import { AppServiceType } from "../utils/types";

export interface ServiceSetupProps {
	services: Array<AppServiceType>
}

export default function ServiceSetupModal({ services } : ServiceSetupProps) {
	const linkedServices = services.filter(el => el.linked);
	const unlinkedServices = services.filter(el => !el.linked);

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
							Linked
						</Typography>
						<Box
							sx={{
								flexDirection: "column",
								alignItems: "center",
								justifyContent: "space-between",
								marginRight: "10px",
							}}>
							{linkedServices.map((elem, index) => (
								<Grid item mb={4} key={index}>
									<GenericButton service={elem.logo} title={elem.label} trailingIcon={<Logout />} />
								</Grid>
							))}
						</Box>
					</Grid>
				</Grid>
				<Grid item xs={12} sm container>
					<Grid item xs container direction="column" spacing={2}>
						<Grid item xs>
							<Typography variant="h6" noWrap align="right">
								Available
							</Typography>
							<Box
								sx={{
									flexDirection: "column",
									alignItems: "center",
									justifyContent: "space-between",
									marginRight: "10px",
								}}>
								{unlinkedServices.map((elem, index) => (
									<Grid item mb={4} key={index}>
										<GenericButton service={elem.logo} title={elem.label} trailingIcon={<Login />} onClickCallback={() => window.location.href = elem.urlAuth} />
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
