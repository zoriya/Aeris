import { Card, Chip, CardActionArea, CardContent, CardHeader, Typography, Avatar, Grid } from "@mui/material";

import { AppAREAType } from "../utils/types";
import i18next from "i18next";

export interface AREACardProps {
	AREA: AppAREAType;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
}

export const AREACard = ({ AREA, onClick }: AREACardProps) => {
	const languageUid = i18next.resolvedLanguage;
	return (
		<Card sx={{ width: "500px" }}>
			<CardActionArea onClick={onClick}>
				<CardHeader
					avatar={
						<Avatar
							style={{ objectFit: "cover", height: "100%" }}
							alt={AREA.service.logo.altText}
							src={AREA.service.logo.imageSrc}
							variant={"square"}
						/>
					}
					title={<Typography variant="h5">{AREA.type}</Typography>}
					subheader={AREA.description[languageUid]}
				/>
				{Object.keys(AREA.params).length > 0 || Object.keys(AREA.returns).length > 0 ? (
					<CardContent>
						<Grid container spacing={1} marginBottom={"5px"}>
							{Object.entries(AREA.params).map((el, idx) => {
								return (
									<Grid item key={idx}>
										<Chip
											label={el[0]}
											title={el[1].description[languageUid]}
											color="secondary"
											variant="outlined"
											size="small"
										/>
									</Grid>
								);
							})}
						</Grid>

						<Grid container spacing={0.5}>
							{Object.entries(AREA.returns).map((el, idx) => {
								return (
									<Grid item key={idx}>
										<Chip label={el[0]} title={el[1][languageUid]} color="primary" variant="outlined" size="small" />
									</Grid>
								);
							})}
						</Grid>
					</CardContent>
				) : (
					<div></div>
				)}
			</CardActionArea>
		</Card>
	);
};
