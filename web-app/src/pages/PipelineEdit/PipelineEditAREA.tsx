import { InputLabel, FormHelperText, Button } from "@mui/material";
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

export interface PipelineEditAREAProps {
	pipelineData: AppPipelineType;
	services: Array<AppServiceType>;
	AREAs: Array<AppAREAType>;
	setEditMode: any;
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
					gridTemplateColumns: "8fr 1fr",
					gridTemplateRows: "2fr 50vh 1fr",
					gridTemplateAreas: `
							'mainTitle 	select'
							'AREAData 	AREAData'
							'.			buttonBack'
					`,
					placeItems: "center",
				}}>
				<Typography gridArea={"mainTitle"} justifySelf={"left"} variant="h4" noWrap align="left">
					Setup {AREAs[0].isAction ? "Action" : "Réaction"} :
				</Typography>

				<Box sx={{ gridArea: "select" }}>
					<InputLabel id="pipeline-setup-select-label">Service</InputLabel>
					<Select
						labelId="pipeline-setup-select-label"
						defaultValue={serviceToShow}
						variant="standard"
						onChange={(newValue) => setServiceToShow(newValue.target.value)}
						label="Services">
						{services.map((item) => (
							<MenuItem value={item.uid}>
								<Box sx={{ display: "flex", alignItems: "center", justifyContent: "space-between" }}>
									<img loading="lazy" width="20" src={item.logo.imageSrc} alt={item.logo.altText} />
									{item.label}
								</Box>
							</MenuItem>
						))}
					</Select>
					<FormHelperText>{filteredElements.length} actions disponibles</FormHelperText>
				</Box>

				<Grid
					container
					gridArea={"AREAData"}
					alignSelf="start"
					direction="row"
					justifyContent="flex-start"
					spacing={2}
					alignItems="flex-start">
					{filteredElements.map((el, elIndex) => {
						return (
							<Grid item key={elIndex}>
								<GenericButton
									title={el.type}
									service={el.service.logo}
									trailingIcon={<MoreVertIcon />}
									onClickCallback={() => {
										setAREAData(el);
										setIsOpenParamsModal(true);
									}}
								/>
							</Grid>
						);
					})}
				</Grid>
				
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
