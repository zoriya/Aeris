import 'package:aeris/src/widgets/reorderable_reaction_cards_list.dart';
import 'package:flutter/material.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/views/pipeline_detail_page.dart';
import 'package:aeris/src/views/setup_action_page.dart';
import 'package:aeris/src/widgets/action_card.dart';
import 'package:aeris/src/widgets/action_card_popup_menu.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:aeris/src/widgets/colored_clickable_card.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:recase/recase.dart';

/// Page to create a new pipeline
class CreatePipelinePage extends StatefulWidget {
  const CreatePipelinePage({Key? key}) : super(key: key);

  @override
  State<CreatePipelinePage> createState() => _CreatePipelinePageState();
}

class _CreatePipelinePageState extends State<CreatePipelinePage> {
  /// Creates a basic Template the user can modify
  Trigger trigger = Trigger.template();

  /// Gets the list of reactions the pipeline can have
  List<Reaction> reactions = [];

  /// Pipeline name
  String? name;

  @override
  Widget build(BuildContext context) {
    final _formKey = GlobalKey<FormBuilderState>();
    return Consumer<PipelineProvider>(
        builder: (context, provider, _) => AerisCardPage(
                body: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
              Text(AppLocalizations.of(context).createNewPipeline,
                  style: const TextStyle(
                    fontSize: 25,
                  )),
              FormBuilder(
                  key: _formKey,
                  child: Padding(
                    padding: const EdgeInsets.all(20),
                    child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                      FormBuilderTextField(
                        name: 'name',
                        initialValue: name,
                        decoration: InputDecoration(
                          labelText:
                              AppLocalizations.of(context).nameOfThePipeline,
                        ),
                        validator: FormBuilderValidators.compose([
                          FormBuilderValidators.required(context),
                          FormBuilderValidators.minLength(context, 5),
                        ]),
                        onChanged: (value) {
                          name = value;
                        },
                      ),
                      const SizedBox(height: 10),
                      trigger != Trigger.template()
                      ? Text(AppLocalizations.of(context).action,
                        style: const TextStyle(fontWeight: FontWeight.w500))
                      : Container(),
                      Padding(
                        padding: const EdgeInsets.all(8.0),
                        child: trigger == Trigger.template()
                            ? ColoredClickableCard(
                                color: Theme.of(context)
                                    .colorScheme
                                    .secondaryContainer,
                                text: AppLocalizations.of(context).addTrigger,
                                onTap: () {
                                  showAerisCardPage(
                                          context,
                                          (_) =>
                                              SetupActionPage(action: trigger))
                                      .then((_) => setState(() {}));
                                })
                            : ActionCard(
                                leading: trigger.service.getLogo(logoSize: 50),
                                title: ReCase(trigger.name).titleCase,
                                trailing: ActionCardPopupMenu(
                                    deletable: false,
                                    action: trigger,
                                    then: () => setState(() {})),
                              ),
                      ),
                      reactions.isNotEmpty
                        ? Text(AppLocalizations.of(context).reactions,
                          style: const TextStyle(fontWeight: FontWeight.w500))
                        : Container(),
                        Padding(
                            padding: const EdgeInsets.only(left: 8, right: 8),
                            child:  ReorderableReactionCardsList(
                              reactionList: reactions,
                              onReorder: () {  },
                              itemBuilder: (reaction) => ActionCard(
                              key: ValueKey(reactions.indexOf(reaction)),
                              leading: reaction.service.getLogo(logoSize: 50),
                              title: ReCase(reaction.name).titleCase,
                              trailing: ActionCardPopupMenu(
                                  deletable: reactions.length > 1,
                                  action: reaction,
                                  then: () => setState(() {}),
                                  onDelete: () {
                                    setState(() {
                                      reactions.remove(reaction);
                                    });
                                  })),
                          )
                        ),
                      Padding(
                        padding: const EdgeInsets.all(8.0),
                        child: ColoredClickableCard(
                            color: Theme.of(context)
                                .colorScheme
                                .secondaryContainer,
                            text: AppLocalizations.of(context).addReaction,
                            onTap: () async {
                              var newreact = Reaction.template();
                              showAerisCardPage(
                                      context,
                                      (_) => SetupActionPage(
                                          action: newreact))
                                  .then((_) => setState(() {
                                    if (newreact != Reaction.template()) {
                                      reactions.add(newreact);
                                    }
                                  }));
                            }),
                      ),
                      Center(child: ElevatedButton(
                        child: Text(AppLocalizations.of(context).save),
                        onPressed: () {
                          _formKey.currentState!.save();
                          if (_formKey.currentState!.validate()) {
                            if (trigger == Trigger.template() ||
                                reactions.isEmpty ||
                                reactions
                                    .where((element) =>
                                        element == Reaction.template())
                                    .isNotEmpty) {
                              ScaffoldMessenger.of(context).showSnackBar(SnackBar(
                                  backgroundColor:
                                      Theme.of(context).colorScheme.secondary,
                                  content: const Text(
                                      "You must select at least a trigger and a reaction")));
                            } else {
                              Pipeline newPipeline = Pipeline(
                                  id: 0,
                                  name: _formKey.currentState!.value['name'],
                                  triggerCount: 0,
                                  enabled: true,
                                  trigger: trigger,
                                  reactions: reactions);
                              provider.addPipeline(newPipeline);
                              Navigator.of(context).pop();
                              showAerisCardPage(
                                context,
                                (_) => PipelineDetailPage(pipeline: newPipeline)
                              );
                            }
                          }
                        },
                      )),
                    ]),
                  )),
            ])));
  }
}
