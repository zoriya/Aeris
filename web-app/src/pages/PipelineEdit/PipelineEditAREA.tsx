import { InputLabel, FormHelperText, Avatar, ListItemAvatar } from "@mui/material";
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

export interface PipelineEditAREAProps {
	pipelineData: AppPipelineType,
	services: Array<AppServiceType>,
	AREAs: Array<AppAREAType>,
	setEditMode: any,
	setAREA: any
}

export default function PipelineEditAREA({ pipelineData, services, AREAs, setEditMode, setAREA }: PipelineEditAREAProps) {
	const [serviceToShow, setServiceToShow] = useState<string>(services[0].uid);
	const [isOPenParamsModal, setIsOpenParamsModal] = useState<boolean>(false);


	let filteredElements = AREAs.filter(el => el.service.uid === serviceToShow);

	const [AREAData, setAREAData] = useState<AppAREAType>();


	return (
		<div>
			<Box
				sx={{
					width: "100%",
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
				}}>
				<Typography variant="h4" noWrap align="left">
					Setup {AREAs[0].isAction ? "Action" : "Réaction"} :
				</Typography>
			</Box>
			<Box sx={{ float: "right" }}>
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
			<Grid container direction="row" justifyContent="flex-start" spacing={2} alignItems="flex-start">
				{filteredElements.map((el, elIndex) => {
					return (
						<Grid item key={elIndex}>
							<GenericButton 
								title={el.type}
								service={el.service.logo}
								trailingIcon={<MoreVertIcon/>}
								onClickCallback={() => {
									setAREAData(el);
									setIsOpenParamsModal(true)
								}} />
						</Grid>
					);
				})}
			</Grid>

				{ isOPenParamsModal ?
			(<PipelineModal
				isOpen={isOPenParamsModal}
				handleClose={() => setIsOpenParamsModal(false)}
			>
				<PipelineEditParams 
					pipelineData={pipelineData}
					AREA={AREAData ?? filteredElements[0]}
					handleQuit={() => setIsOpenParamsModal(false)}
					setParams={(AREA: AppAREAType) => setAREA(AREA)} />
			</PipelineModal>) : <div></div>
			}
		</div>
	);
}
