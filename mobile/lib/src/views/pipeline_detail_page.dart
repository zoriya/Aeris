import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/views/setup_action_page.dart';
import 'package:aeris/src/widgets/action_card_popup_menu.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:aeris/src/widgets/colored_clickable_card.dart';
import 'package:aeris/src/widgets/warning_dialog.dart';
import 'package:aeris/src/widgets/action_card.dart';
import 'package:drag_and_drop_lists/drag_and_drop_lists.dart';
import 'package:flutter_switch/flutter_switch.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:flutter/material.dart';
import 'package:get_it/get_it.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

///Page for a Pipeline's details
class PipelineDetailPage extends StatefulWidget {
  final Pipeline pipeline;
  const PipelineDetailPage({Key? key, required this.pipeline})
      : super(key: key);

  @override
  State<PipelineDetailPage> createState() => _PipelineDetailPageState();
}

class _PipelineDetailPageState extends State<PipelineDetailPage> {
  @override
  Widget build(BuildContext context) =>
      Consumer<PipelineProvider>(builder: (context, provider, _) {
        Pipeline pipeline = widget.pipeline;

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
                            GetIt.I<AerisAPI>().editPipeline(pipeline);
                            provider.sortPipelines();
                          });
                        },
                      ),
                    ),
                    const SizedBox(height: 10),
                    Align(
                      alignment: Alignment.center,
                      child: Text(
                          pipeline.enabled
                              ? AppLocalizations.of(context).enabled
                              : AppLocalizations.of(context).disabled,
                          style: const TextStyle(fontSize: 13)),
                    ),
                  ],
                ))
          ],
        );

        final Widget addReactionbutton = ColoredClickableCard(
            color: Theme.of(context).colorScheme.secondaryContainer,
            text: AppLocalizations.of(context).addReaction,
            onTap: () {
              Reaction newreaction = Reaction.template();
              showAerisCardPage(
                      context, (_) => SetupActionPage(action: newreaction))
                  .then((r) {
                if (newreaction != Reaction.template()) {
                  setState(() {
                    pipeline.reactions.add(newreaction);
                    GetIt.I<AerisAPI>().editPipeline(pipeline);
                  });
                }
                return r;
              });
            });

        final Widget deleteButton = ColoredClickableCard(
          color: Theme.of(context).colorScheme.error,
          text: AppLocalizations.of(context).deletePipeline,
          onTap: () => showDialog<String>(
              context: context,
              builder: (BuildContext context) => WarningDialog(
                  message:
                      AppLocalizations.of(context).deletePipelineWarningMessage,
                  onAccept: () {
                    provider.removePipeline(pipeline);
                    Navigator.of(context).pop();
                  },
                  warnedAction: AppLocalizations.of(context).delete)),
        );

        return AerisCardPage(
            body: Padding(
          padding: const EdgeInsets.only(top: 10),
          child:
              Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
            Padding(
              padding: const EdgeInsets.only(bottom: 40),
              child: cardHeader,
            ),
            Text(AppLocalizations.of(context).action,
                style: const TextStyle(fontWeight: FontWeight.w500)),
            ActionCard(
                leading: pipeline.trigger.service.getLogo(logoSize: 50),
                title: pipeline.trigger.name,
                trailing: ActionCardPopupMenu(
                    deletable: false,
                    action: pipeline.trigger,
                    then: () {
                      setState(() {});
                      GetIt.I<AerisAPI>().editPipeline(pipeline);
                    })),
            const SizedBox(height: 25),
            Text(AppLocalizations.of(context).reactions,
                style: const TextStyle(fontWeight: FontWeight.w500)),
            DragAndDropLists(
              disableScrolling: true,
              onItemReorder: (int oldItemIndex, _, int newItemIndex, __) {
                setState(() {
                  var movedItem = pipeline.reactions.removeAt(oldItemIndex);
                  pipeline.reactions.insert(newItemIndex, movedItem);
                  GetIt.I<AerisAPI>().editPipeline(pipeline);
                });
              },
              onListReorder: (_,__) {},
              children: [DragAndDropList(children: [...[
                for (var reaction in pipeline.reactions)
                DragAndDropItem(
                  canDrag: pipeline.reactions.length > 1,
                  child: ActionCard(
                  leading: reaction.service.getLogo(logoSize: 50),
                  title: reaction.name,
                  trailing: ActionCardPopupMenu(
                      deletable: pipeline.reactions.length > 1,
                      action: reaction,
                      then: () {
                        setState(() {});
                        GetIt.I<AerisAPI>().editPipeline(pipeline);
                      },
                      onDelete: () {
                        pipeline.reactions.remove(reaction);
                        setState(() {});
                        GetIt.I<AerisAPI>().editPipeline(pipeline);
                      }),
                )
                )]],
            )]),
            addReactionbutton,
            Padding(
                padding: const EdgeInsets.only(top: 30, bottom: 5),
                child: Text(AppLocalizations.of(context).dangerZone,
                    style: const TextStyle(fontWeight: FontWeight.w500))),
            deleteButton,
            const SizedBox(height: 25),
          ]),
        ));
      });
}
