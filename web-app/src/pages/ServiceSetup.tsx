import Typography from "@mui/material/Typography";
import { Login, Logout } from "@mui/icons-material";
import Box from "@mui/material/Box";

import GenericButton from "../components/GenericButton";
import { AppServiceType } from "../utils/types";
import { Grid } from "@mui/material";
import { unLinkService } from "../utils/utils";
import { useTranslation } from "react-i18next";
import "../i18n/config";

export interface ServiceSetupProps {
	services: Array<AppServiceType>;
	setServices: (srvcs: Array<AppServiceType>) => any;
}

export default function ServiceSetupModal({ services, setServices }: ServiceSetupProps) {
	const { t } = useTranslation();
	services = services.filter(x => x.uid !== "utils");
	const unlinkedServices = services.filter((el) => !el.linked);
	const linkedServices = services.filter((el) => el.linked);
	return (
		<div
			style={{
				display: "grid",
				gridTemplateColumns: "30vw 5vw 30vw",
				gridTemplateRows: "2fr 1fr auto",
				gridTemplateAreas: `
					'WindowTitle  WindowTitle WindowTitle'
					'LinkedTitle  .           UnlinkedTitle'
					'LinkedList   .           UnlinkedList'
				`,
				alignItems: "center",
			}}>
			<Box
				sx={{
					gridArea: "WindowTitle",
				}}>
				<Typography variant="h4" noWrap align="left">
					Services
				</Typography>
			</Box>

			<Typography sx={{ gridArea: "LinkedTitle" }} variant="h6" noWrap>
				{t("linked")}
			</Typography>

			<Grid gridArea={"LinkedList"} alignSelf="start" container direction="column" spacing={2}>
				{linkedServices.map((elem, index) => (
					<Grid item key={index}>
						<GenericButton
							service={elem.logo}
							title={elem.label}
							trailingIcon={<Logout />}
							onClickCallback={async () => {
								if (await unLinkService(elem)) {
									setServices(
										services.map((svc) => {
											if (svc.uid !== elem.uid) return svc;
											return {
												...svc,
												linked: false,
											};
										})
									);
								}
							}}
						/>
					</Grid>
				))}
			</Grid>

			<Typography sx={{ gridArea: "UnlinkedTitle" }} variant="h6" noWrap>
				{t("available")}
			</Typography>
			<Grid
				sx={{
					gridArea: "UnlinkedList",
					alignSelf: "start",
				}}
				container
				direction="column"
				spacing={2}>
				{unlinkedServices.map((elem, index) => (
					<Grid item key={index}>
						<GenericButton
							service={elem.logo}
							title={elem.label}
							trailingIcon={<Login />}
							onClickCallback={() => (window.location.href = elem.urlAuth)}
						/>
					</Grid>
				))}
			</Grid>
		</div>
	);
}
