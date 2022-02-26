import { InputLabel, FormHelperText, Button, SelectChangeEvent } from "@mui/material";
import Typography from "@mui/material/Typography";
import MenuItem from "@mui/material/MenuItem";
import Select from "@mui/material/Select";
import Grid from "@mui/material/Grid";
import Box from "@mui/material/Box";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { AppServiceType, AppAREAType, AppPipelineType } from "../../utils/types";
import GenericButton, { GenericButtonProps } from "../../components/GenericButton";
import PipelineModal from "../../components/Pipelines/PipelineModal";
import PipelineEditParams from "./PipelineEditParams";
import { useState } from "react";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import { PipelineEditMode } from "./PipelineEditPage";
import { AREACard } from "../../components/AREACard";

export interface PipelineEditAREAProps {
	pipelineData: AppPipelineType;
	services: Array<AppServiceType>;
	AREAs: Array<AppAREAType>;
	setEditMode: (mode: PipelineEditMode) => any;
	setAREA: any;
}

export default function PipelineEditAREA({
	pipelineData,
	services,
	AREAs,
	setEditMode,
	setAREA,
}: PipelineEditAREAProps) {
	const [serviceToShow, setServiceToShow] = useState<string>(services[0].uid);
	const [isOPenParamsModal, setIsOpenParamsModal] = useState<boolean>(false);

	let filteredElements = AREAs.filter((el) => el.service.uid === serviceToShow);

	const [AREAData, setAREAData] = useState<AppAREAType>();

	return (
		<div>
			<div
				style={{
					display: "grid",
					gridTemplateColumns: "50vw 1fr",
					gridTemplateRows: "3fr 50vh 1fr",
					gridTemplateAreas: `
							'mainTitle  select'
							'AREAData   AREAData'
							'.          buttonBack'
					`,
					placeItems: "center",
				}}>
				<Typography gridArea={"mainTitle"} justifySelf={"left"} width="100%" variant="h4" noWrap align="left">
					Setup {"Réaction"} :
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
					<FormHelperText>{filteredElements.length} actions disponibles</FormHelperText>
				</Box>

				<div style={{
					gridArea: "AREAData",
					maxHeight: "50vh",
					width: "60vw",
					overflow: "auto",
					padding: "10px"
				}}>
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
								<AREACard AREA={el} onClick={() => {
										setAREAData(el);
										setIsOpenParamsModal(true);
									}} />
							</Grid>
						);
					})}
				</Grid>
				</div>

				<Button
					sx={{ gridArea: "buttonBack" }}
					color="secondary"
					startIcon={<ArrowBackIcon />}
					onClick={() => setEditMode(PipelineEditMode.Pipeline)}
					variant="contained">
					Retour
				</Button>
			</div>

			{isOPenParamsModal ? (
				<PipelineModal isOpen={isOPenParamsModal} handleClose={() => setIsOpenParamsModal(false)}>
					<PipelineEditParams
						pipelineData={pipelineData}
						AREA={AREAData ?? filteredElements[0]}
						handleQuit={() => setIsOpenParamsModal(false)}
						setParams={(AREA: AppAREAType) => setAREA(AREA)}
					/>
				</PipelineModal>
			) : (
				<div></div>
			)}
		</div>
	);
}
