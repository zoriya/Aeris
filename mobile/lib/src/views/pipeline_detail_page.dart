import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/views/setup_action_page.dart';
import 'package:mobile/src/widgets/action_card_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_popup_menu_item.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/colored_clickable_card.dart';
import 'package:mobile/src/widgets/warning_dialog.dart';
import 'package:mobile/src/models/action.dart' as aeris;
import 'package:mobile/src/widgets/action_card.dart';
import 'package:flutter_switch/flutter_switch.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

/// Class to get the pipeline's name in route's arguments
class PipelineDetailPageArguments {
  final Pipeline pipeline;

  ///TODO Should be later defined as an int, to fetch from db, or as the object

  PipelineDetailPageArguments(this.pipeline);
}

///Page for a Pipeline's details
class PipelineDetailPage extends StatefulWidget {
  //final String pipelineName; // TODO Define as int later on
  const PipelineDetailPage({Key? key}) : super(key: key);

  @override
  State<PipelineDetailPage> createState() => _PipelineDetailPageState();
}

class _PipelineDetailPageState extends State<PipelineDetailPage> {
  @override
  Widget build(BuildContext context) =>
      Consumer<PipelineProvider>(builder: (context, provider, _) {
        final PipelineDetailPageArguments arguments = ModalRoute.of(context)!
            .settings
            .arguments as PipelineDetailPageArguments;

        Pipeline pipeline = arguments.pipeline;
        final cardHeader = Row(
          children: [
            Expanded(
              flex: 7,
              child: Column(
                children: [
                  Align(
                    alignment: Alignment.centerLeft,
                    child: Text(pipeline.name,
                        style: const TextStyle(
                          fontSize: 25,
                        )),
                  ),
                  const SizedBox(height: 10),
                  Align(
                    alignment: Alignment.centerLeft,
                    child: Text(pipeline.trigger.lastToString(),
                        style: const TextStyle(
                          fontSize: 17,
                        )),
                  ),
                ],
              ),
            ),
            Expanded(
                flex: 3,
                child: Column(
                  children: [
                    const SizedBox(height: 10),
                    Align(
                      alignment: Alignment.center,
                      child: FlutterSwitch(
                        activeColor: Colors.green,
                        width: 60,
                        value: pipeline.enabled,
                        onToggle: (value) {
                          setState(() {
                            pipeline.enabled = !pipeline.enabled;
                            provider.sortPipelines();
                            provider.notifyListeners();
                            // TODO call api
                          });
                        },
                      ),
                    ),
                    const SizedBox(height: 10),
                    Align(
                      alignment: Alignment.center,
                      child: Text(pipeline.enabled ? "Enabled" : "Disabed",
                          style: const TextStyle(fontSize: 13)),
                    ),
                  ],
                ))
          ],
        );

        final Widget addReactionbutton = ColoredClickableCard(
            color: Theme.of(context).colorScheme.secondaryContainer,
            text: "Add a reaction",
            onTap: () {
              print("add reaction pipeline"); // TODO add reaction
            });

        final Widget deleteButton = ColoredClickableCard(
          color: Theme.of(context).colorScheme.error,
          text: "Delete a Pipeline",
          onTap: () => showDialog<String>(
              context: context,
              builder: (BuildContext context) => WarningDialog(
                  message:
                      "You are about to delete a pipeline. This action can not be undone. Are you sure ?",
                  onAccept: () {
                    provider.removePipeline(pipeline);
                    print("Delete pipeline"); /*TODO call api*/
                    Navigator.of(context).pop();
                  },
                  warnedAction: "Delete")),
        );

        return AerisCardPage(
            body: Padding(
          padding: const EdgeInsets.only(top: 10),
          child: ListView(children: [
            Padding(
              padding: const EdgeInsets.only(bottom: 40),
              child: cardHeader,
            ),
            const Text("Action", style: TextStyle(fontWeight: FontWeight.w500)),
            ActionCard(
                leading: pipeline.trigger.service.getLogo(logoSize: 50),
                title: pipeline.trigger.name,
                trailing: ActionCardPopupMenu(deletable: false, action: pipeline.trigger, then: () => setState(() {}))),
            const SizedBox(height: 25),
            const Text("Reactions",
                style: TextStyle(fontWeight: FontWeight.w500)),
            for (var reaction in pipeline.reactions)
              ActionCard(
                  leading: reaction.service.getLogo(logoSize: 50),
                  title: reaction.name,
                  trailing: ActionCardPopupMenu(deletable: reaction != pipeline.reactions.first ,action: reaction, then: () => setState(() {}))),
            addReactionbutton,
            const Padding(
                padding: EdgeInsets.only(top: 30, bottom: 5),
                child: Text("Danger Zone",
                    style: TextStyle(fontWeight: FontWeight.w500))),
            deleteButton,
            const SizedBox(height: 25),
          ]),
        ));
      });
}
