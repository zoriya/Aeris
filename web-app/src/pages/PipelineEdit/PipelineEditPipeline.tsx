import { AppAREAType, AppPipelineType } from "../../utils/types";
import {
	Button,
	ButtonGroup,
	FormControlLabel,
	FormGroup,
	Grid,
	IconButton,
	Switch,
	TextField,
	Typography,
} from "@mui/material";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import CloseIcon from "@mui/icons-material/Close";
import EditIcon from "@mui/icons-material/Edit";
import SaveIcon from "@mui/icons-material/Save";
import LoadingButton from "@mui/lab/LoadingButton";
import { PipelineEditMode } from "./PipelineEditPage";
import { PipelineAREACard } from "../../components/PipelineAREACard";
import { deepCopy } from "../../utils/utils";

import { useTranslation } from "react-i18next";
import "../../i18n/config";
import { NoAREA } from "../../utils/globals";
import { useState } from "react";

import { DragDropContext, Draggable, Droppable, DropResult } from "react-beautiful-dnd";

interface PipelineEditPipelineProps {
	pipelineData: AppPipelineType;
	handleEditPipelineMetaData: (name: string, enblaed: boolean) => any;
	handleEditAction: (action: AppAREAType) => any;
	handleEditReaction: (reaction: AppAREAType, index: number) => any;
	handleDeleteReaction: (reaction: AppAREAType, index: number) => any;
	handleDelete: (pD: AppPipelineType) => any;
	handleEditReactionOrder: (pDs: Array<AppAREAType>) => any;
	handleSave: (pD: AppPipelineType) => any;
	handleEditPipelineTitle: (newTtitle: string) => any;
	setEditMode: (mode: PipelineEditMode) => any;
	setEditReactionIndex: any;
	disableDeletion: boolean;
}

export default function PipelineEditPipeline({
	pipelineData,
	handleEditReaction,
	handleEditPipelineMetaData,
	handleEditAction,
	handleDeleteReaction,
	handleEditPipelineTitle,
	handleDelete,
	handleSave,
	handleEditReactionOrder,
	setEditMode,
	disableDeletion,
	setEditReactionIndex,
}: PipelineEditPipelineProps) {
	const { t } = useTranslation();
	const [titleEditMode, setTitleEditMode] = useState<boolean>(false);
	const [reactionsList, setReactionList] = useState<AppAREAType[]>(pipelineData.reactions);
	const [titlePipelineEditValue, setTitlePipelineEditValue] = useState<string>(pipelineData.name);

	const handleOnDragEnd = (result: DropResult) => {
		const { source, destination } = result;

		if (!destination) return;
		const items = deepCopy(reactionsList);
		const [newOrder] = items.splice(source.index, 1);
		items.splice(destination.index, 0, newOrder);
		setReactionList(items);
		handleEditReactionOrder(items);
	};

	return (
		<div>
			<div
				style={{
					display: "grid",
					gridTemplateColumns: "25vw 5vw 12vw 13vw",
					gridTemplateRows: "100px 1fr auto 3fr 1fr",
					gridTemplateAreas: `
							'pipelineTitle  pipelineTitle   pipelineTitle       enabledStatus'
							'actionTitle    .               reactionTitle       reactionTitle'
							'actionData     arrow           reactionData        reactionData'
							'.              .               buttonAddReaction   buttonAddReaction'
							'buttonDelete   .               buttonCancelSave    buttonCancelSave'
						`,
					justifyItems: "center",
					alignItems: "center",
				}}>
				<div style={{ width: "100%", gridArea: "pipelineTitle", justifySelf: "left", display: "flex" }}>
					<IconButton color="secondary" aria-label="Edit title" onClick={() => setTitleEditMode(!titleEditMode)}>
						{titleEditMode ? <CloseIcon /> : <EditIcon />}
					</IconButton>
					{titleEditMode ? (
						<TextField
							autoFocus
							inputProps={{ style: { fontSize: "3.75rem" } }}
							fullWidth
							onChange={(e) => setTitlePipelineEditValue(e.target.value)}
							onKeyPress={(e) => {
								if (e.key === "Enter") {
									handleEditPipelineTitle(titlePipelineEditValue);
									setTitleEditMode(false);
								}
							}}
							variant="standard"
							defaultValue={pipelineData.name}
						/>
					) : (
						<Typography width="calc(100% - 50px)" variant="h2" noWrap align="left">
							{pipelineData.name}
						</Typography>
					)}
				</div>

				<FormGroup style={{ gridArea: "enabledStatus" }}>
					<FormControlLabel
						control={
							<Switch
								color="secondary"
								checked={pipelineData.data.enabled}
								onChange={(e) => {
									handleEditPipelineMetaData(pipelineData.name, e.target.checked);
								}}
							/>
						}
						label={t(pipelineData.data.enabled ? "activated" : "deactivated") as string}
					/>
				</FormGroup>

				<Typography style={{ gridArea: "actionTitle", justifySelf: "left" }} variant="h5" noWrap align="left">
					{t("actionCaps")}
				</Typography>

				<Typography style={{ gridArea: "reactionTitle", justifySelf: "left" }} variant="h5" noWrap align="left">
					{t("reactionCaps")}
				</Typography>

				<Grid
					container
					gridArea={"actionData"}
					direction="column"
					spacing={2}
					justifyContent="flex-start"
					alignItems="flex-start">
					<Grid item sm={10} md={10} lg={5} xl={4}>
						{pipelineData.action.type === NoAREA.type ? (
							<Grid item sm={10} md={10} lg={5} xl={4}>
								<Button
									sx={{ width: "25vw" }}
									variant={"contained"}
									color={"secondary"}
									onClick={() => setEditMode(PipelineEditMode.Action)}>
									{t("addAction")}
								</Button>
							</Grid>
						) : (
							<PipelineAREACard
								canBeRemoved={false}
								handleEdit={() => {
									setEditMode(PipelineEditMode.Action);
								}}
								handleDelete={() => {}}
								AREA={pipelineData.action}
								style={{ width: "25vw" }}
								order={0}
								onClick={() => {}}
							/>
						)}
					</Grid>
				</Grid>

				<ArrowForwardIcon sx={{ gridArea: "arrow", height: 38, width: 38 }} />

				<div
					style={{
						width: "100%",
						overflow: "auto",
						maxHeight: "50vh",
						gridArea: "reactionData",
						padding: "10px",
					}}>
					<Grid container direction="column" spacing={1} justifyContent="center" alignItems="flex-start">
						{pipelineData.reactions.length === 0 && (
							<Grid item sm={10} md={10} lg={5} xl={4}>
								<Button
									sx={{ width: "24.5vw" }}
									variant={"contained"}
									color={"secondary"}
									onClick={() => {
										setEditMode(PipelineEditMode.Reactions);
										setEditReactionIndex(pipelineData.reactions.length);
									}}>
									{t("addReaction")}
								</Button>
							</Grid>
						)}
						<DragDropContext onDragEnd={handleOnDragEnd}>
							<Droppable droppableId="reactions">
								{(provided) => (
									<div className="reactions" {...provided.droppableProps} ref={provided.innerRef}>
										{reactionsList.map((el, index, arr) => {
											return (
												<Draggable key={index.toString()} draggableId={index.toString()} index={index}>
													{(providedDrag, snapshot) => (
														<Grid
															item
															sm={10}
															marginBottom={1}
															md={10}
															lg={5}
															xl={4}
															key={index}
															ref={providedDrag.innerRef}
															{...providedDrag.draggableProps}
															{...providedDrag.dragHandleProps}>
															<PipelineAREACard
																AREA={el}
																order={index + 1}
																canBeRemoved={arr.length > 1}
																handleEdit={() => {
																	setEditMode(PipelineEditMode.EditReaction);
																	handleEditReaction(el, index);
																}}
																handleDelete={() => {
																	handleDeleteReaction(el, index);
																}}
																onClick={() => {}}
																style={{ width: "24.5vw" }}
															/>
														</Grid>
													)}
												</Draggable>
											);
										})}
										{provided.placeholder}
									</div>
								)}
							</Droppable>
						</DragDropContext>
					</Grid>
				</div>

				{pipelineData.reactions.length !== 0 && (
					<LoadingButton
						sx={{ gridArea: "buttonAddReaction" }}
						color="secondary"
						loading={false}
						loadingPosition="start"
						onClick={() => {
							setEditMode(PipelineEditMode.Reactions);
							setEditReactionIndex(pipelineData.reactions.length);
						}}
						startIcon={<AddBoxIcon />}
						variant="contained">
						{t("addReaction")}
					</LoadingButton>
				)}

				<LoadingButton
					sx={{ gridArea: "buttonDelete", justifySelf: "left" }}
					variant="contained"
					color="error"
					startIcon={<DeleteIcon />}
					loadingPosition="start"
					onClick={() => handleDelete(pipelineData)}
					disabled={disableDeletion}
					loading={false}>
					{t("deletePipeline")}
				</LoadingButton>

				<ButtonGroup sx={{ gridArea: "buttonCancelSave", justifySelf: "right" }}>
					<Button
						color="primary"
						startIcon={<SaveIcon />}
						disabled={pipelineData.action.type === NoAREA.type || pipelineData.reactions.length === 0}
						onClick={async () => handleSave(pipelineData)}
						variant="contained">
						{t("save")}
					</Button>
				</ButtonGroup>
			</div>
		</div>
	);
}
