import { InputLabel, FormHelperText, Button, SelectChangeEvent, Divider } from "@mui/material";
import Typography from "@mui/material/Typography";
import MenuItem from "@mui/material/MenuItem";
import Select from "@mui/material/Select";
import Grid from "@mui/material/Grid";
import Box from "@mui/material/Box";

import { AppServiceType, AppAREAType, AppPipelineType } from "../../utils/types";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import PipelineEditParams from "./PipelineEditParams";
import { PipelineEditMode } from "./PipelineEditPage";
import { AREACard } from "../../components/AREACard";
import { useState } from "react";

import { useTranslation } from "react-i18next";
import "../../i18n/config";

export interface PipelineEditAREAProps {
	pipelineData: AppPipelineType;
	services: Array<AppServiceType>;
	AREAs: Array<AppAREAType>;
	isActions: boolean;
	selectedAREA?: AppAREAType;
	setEditMode: (mode: PipelineEditMode) => any;
	setAREA: (AREA: AppAREAType) => any;
}

export default function PipelineEditAREA({
	pipelineData,
	services,
	AREAs,
	selectedAREA,
	isActions,
	setEditMode,
	setAREA,
}: PipelineEditAREAProps) {
	const { t } = useTranslation();
	const [serviceToShow, setServiceToShow] = useState<string>(selectedAREA?.service.uid ?? services[0].uid);
	const serviceData = services.find((s) => s.uid === serviceToShow);

	let filteredElements = AREAs.filter((el) => el.service.uid === serviceToShow);

	const [AREAData, setAREAData] = useState<AppAREAType | null>(selectedAREA ?? null);

	return (
		<div>
			<div
				style={{
					display: "grid",
					gridTemplateColumns: "60vw 300px",
					gridTemplateRows: "100px auto 50px",
					gridTemplateAreas: `
							'mainTitle  select'
							'AREAData   AREAParams'
							'AREAData   buttonBack'
					`,
					placeItems: "center",
				}}>
				<Typography gridArea={"mainTitle"} justifySelf={"left"} width="100%" variant="h4" noWrap align="left">
					{t("setup")} {isActions ? t("action") : t("reaction")} :
				</Typography>

				<Box sx={{ gridArea: "select" }}>
					<InputLabel id="pipeline-setup-select-label">Service</InputLabel>
					<Select
						labelId="pipeline-setup-select-label"
						variant="standard"
						value={serviceToShow}
						onChange={(newValue: SelectChangeEvent) => setServiceToShow(newValue.target.value as string)}
						label="Services">
						{services.map((item, key) => (
							<MenuItem value={item.uid} key={key}>
								<Box sx={{ display: "flex", alignItems: "center", justifyContent: "space-between" }}>
									<img loading="lazy" width="20" src={item.logo.imageSrc} alt={item.logo.altText} />
									{item.label}
								</Box>
							</MenuItem>
						))}
					</Select>
					<FormHelperText>
						{filteredElements.length} {isActions ? t("action") : t("reactions")} {t("availables")}
					</FormHelperText>
				</Box>

				<div
					style={{
						placeSelf: "start right",
						gridArea: "AREAData",
						height: "calc(50vh + 50px)",
						width: "59vw",
						overflow: "auto",
						padding: "10px",
						borderStyle: "none solid none none",
						borderWidth: "1px",
					}}>
					{serviceData?.linked ? (
						<Grid
							container
							alignSelf="start"
							direction="row"
							justifyContent="flex-start"
							spacing={2}
							alignItems="flex-start">
							{filteredElements.map((el, elIndex) => {
								return (
									<Grid item key={elIndex}>
										<AREACard
											AREA={el}
											onClick={() => {
												setAREAData(el);
											}}
										/>
									</Grid>
								);
							})}
						</Grid>
					) : (
						<Typography
							style={{
								width: "100%",
								height: "100%",
								display: "flex",
								justifyContent: "center",
								alignItems: "center",
							}}>
							{"Vous devez être connecté au service " +
								serviceData?.label +
								" pour profitez de ses différentes actions"}
						</Typography>
					)}
				</div>

				{AREAData === null ? (
					<Typography
						sx={{
							gridArea: "AREAParams",
						}}>
						{isActions ? t("selectAction") : t("selectReaction")}
					</Typography>
				) : (
					<div
						style={{
							placeSelf: "start left",
							height: "50vh",
							overflow: "auto",
							width: "290px",
							padding: "10px",
							gridArea: "AREAParams",
						}}>
						<PipelineEditParams
							pipelineData={pipelineData}
							AREA={AREAData ?? filteredElements[0]}
							handleQuit={() => {}}
							setParams={setAREA}
						/>
					</div>
				)}

				<Button
					sx={{ gridArea: "buttonBack", placeSelf: "end right" }}
					color="primary"
					startIcon={<ArrowBackIcon />}
					onClick={() => setEditMode(PipelineEditMode.Pipeline)}
					variant="contained">
					{t("cancel")}
				</Button>
			</div>
		</div>
	);
}
