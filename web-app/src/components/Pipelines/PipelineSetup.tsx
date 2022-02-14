import PipelineModal from './PipelineModal';

import {Google, GitHub, MusicNote, Twitter, YouTube, SvgIconComponent} from '@mui/icons-material';

import ListItemIcon from '@mui/material/ListItemIcon';
import ListItemText from '@mui/material/ListItemText';
import Typography from '@mui/material/Typography';
import MenuItem from '@mui/material/MenuItem';
import Select from '@mui/material/Select';
import Grid from '@mui/material/Grid';
import Box from '@mui/material/Box';

interface PipelineSetupPageProps {
    actionName: string
}

export default function PipelineSetupModal({ actionName } : PipelineSetupPageProps) {

    const servicesItems = [
        {
            name: "Twitter",
            key: "twitter",
            icon: <Twitter />
        },
        {
            name: "GitHub",
            key: "github",
            icon: <GitHub />
        },
        {
            name: "Google",
            key: "google",
            icon: <Google />
        },
        {
            name: "YouTube",
            key: "youtube",
            icon: <YouTube />
        },
        {
            name: "Spotify",
            key: "spotify",
            icon: <MusicNote />
        }
    ]

    //TODO On line 34, need to change number 11 to number of available actions
    return (
        <div>
            <Box
                sx={{ display: 'flex', flexDirection: 'row', alignItems: "center", justifyContent: "space-between", marginBottom:"100px"}}
            >
                <Typography variant="h2" noWrap align="left">
                    Setup Action: { actionName }
                </Typography>
            </Box>
            <Box
                sx={{ display: 'flex', flexDirection: 'row', alignItems: "right", justifyContent: "flex-end", marginBottom:"50px" }}
            >
                <Typography variant="h3" noWrap align="right"
                    sx={{ mr: 4 }}
                >
                    11 available actions for
                </Typography>
                <Select
                    autoWidth
                    variant="standard"
                    defaultValue={"twitter"}
                >
                    { servicesItems.map((item, index) => (
                        <MenuItem value={item.key}>
                            <Box
                                sx={{ display: 'flex', gap: 1 }}
                            >
                                <ListItemIcon>
                                    {item.icon}
                                </ListItemIcon>
                                <ListItemText primary={item.name}/>
                            </Box>
                        </MenuItem>
                    )) }
                </Select>
            </Box>
            <Grid
                container
                rowSpacing={1}
                columnSpacing={{ xs: 1, sm: 2, md: 3 }}
            >
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
                <Grid
                    item
                    xs={4}
                >
                    <Select></Select>
                </Grid>
            </Grid>
        </div>
    )
}