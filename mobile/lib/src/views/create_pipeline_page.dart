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
                    child: Column(children: [
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
                      Padding(
                        padding: const EdgeInsets.all(8.0),
                        child: trigger == Trigger.template()
                            ? ColoredClickableCard(
                                color: Theme.of(context)
                                    .colorScheme
                                    .secondaryContainer,
                                text: AppLocalizations.of(context).addTrigger,
                                onTap: () {
                                  print("add trigger"); // TODO add reaction
                                  showAerisCardPage(
                                          context,
                                          (_) =>
                                              SetupActionPage(action: trigger))
                                      .then((_) => setState(() {}));
                                })
                            : ActionCard(
                                leading: trigger.service.getLogo(logoSize: 50),
                                title: trigger.service.name,
                                trailing: ActionCardPopupMenu(
                                    deletable: false,
                                    action: trigger,
                                    then: () => setState(() {})),
                              ),
                      ),
                      ...[
                        for (Reaction reaction in reactions)
                          ActionCard(
                              leading: reaction.service.getLogo(logoSize: 50),
                              title: reaction.service.name,
                              trailing: ActionCardPopupMenu(
                                  deletable: reaction != reactions[0],
                                  action: reaction,
                                  then: () => setState(() {}),
                                  onDelete: () {
                                    setState(() {
                                      reactions.remove(reaction);
                                    });
                                  }))
                      ],
                      Padding(
                        padding: const EdgeInsets.all(8.0),
                        child: ColoredClickableCard(
                            color: Theme.of(context)
                                .colorScheme
                                .secondaryContainer,
                            text: AppLocalizations.of(context).addReaction,
                            onTap: () async {
                              // TODO add to db
                              reactions.add(Reaction.template());
                              showAerisCardPage(
                                      context,
                                      (_) => SetupActionPage(
                                          action: reactions.last))
                                  .then(setState(() {}));
                            }),
                      ),
                      ElevatedButton(
                        child: const Text("Save"),
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
                                  parameters: {},
                                  trigger: trigger,
                                  reactions: reactions);
                              provider.addPipelineInProvider(newPipeline);

                              ///TODO add to db
                              Navigator.of(context).popAndPushNamed('/pipeline',
                                  arguments:
                                      PipelineDetailPageArguments(newPipeline));
                            }
                          }
                        },
                      ),
                    ]),
                  )),
            ])));
  }
}