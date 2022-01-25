import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/widgets/clickable_card.dart';

/// Widget for Action-reaction card on home page
class PipelineCard extends StatelessWidget {
  // Pipeline base object
  final Pipeline pipeline;

  const PipelineCard({Key? key, required this.pipeline}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    int elapsedDays = DateTime.now().difference(pipeline.trigger.last).inDays;
    List<Widget> reactionLogos = pipeline.reactions
        .take(3)
        .map((reaction) => reaction.service.getLogo())
        .fold<List<Widget>>(
            [],
            (array, logo) =>
                array + [logo, const SizedBox(height: 5)]).toList();
    reactionLogos.removeLast();
    return ClickableCard(
        onTap: () {
          print("Card clicked!");
        },
        color: pipeline.enabled == false ? const Color.fromARGB(115, 34, 34, 34).withOpacity(0.8) : null,
        body: Container(
            width: double.infinity,
            padding: const EdgeInsets.only(top: 20, bottom: 20, left: 40),
            child: Row(children: [
              Expanded(
                  flex: 4,
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(pipeline.name,
                          style: const TextStyle(
                            color: Colors.grey,
                            fontSize: 25,
                          )),
                      const SizedBox(height: 10),
                      Text(
                          elapsedDays == 0
                              ? 'Last: Today'
                              : 'Last: ${elapsedDays.toString()}d ago',
                          style: TextStyle(
                              color: pipeline.enabled == false
                                  ? Colors.grey
                                  : const Color.fromARGB(255, 83, 83, 83),
                              fontSize: 15)),
                    ],
                  )),
              Expanded(
                  flex: 4,
                  child: Row(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        pipeline.trigger.service.getLogo(),
                        const SizedBox(width: 10),
                        const Icon(
                          Icons.arrow_forward,
                          color: Colors.grey,
                        ),
                        const SizedBox(width: 10),
                        Column(children: reactionLogos)
                      ])),
              Expanded(
                  flex: 2,
                  child: Column(
                      children: const [
                        Icon(
                          Icons.arrow_forward_ios,
                          color: Colors.grey,
                        )
                      ],
                      mainAxisAlignment: MainAxisAlignment.center,
                      crossAxisAlignment: CrossAxisAlignment.center)),
            ])));
  }
}
