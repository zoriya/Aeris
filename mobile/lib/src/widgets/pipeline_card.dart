import 'package:flutter/material.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/views/pipeline_detail_page.dart';
import 'package:aeris/src/widgets/clickable_card.dart';

import 'aeris_card_page.dart';

/// Widget for Action-reaction card on home page
class PipelineCard extends StatefulWidget {
  ///Pipeline base object
  final Pipeline pipeline;

  const PipelineCard({Key? key, required this.pipeline}) : super(key: key);

  @override
  State<PipelineCard> createState() => _PipelineCardState();
}

class _PipelineCardState extends State<PipelineCard> {
  @override
  Widget build(BuildContext context) {
    List<Widget> reactionLogos = widget.pipeline.reactions
        .take(3)
        .map((reaction) => reaction.service.getLogo())
        .fold<List<Widget>>(
            [],
            (array, logo) =>
                array + [logo, const SizedBox(height: 5)]).toList();
    reactionLogos.removeLast();
    return ClickableCard(
        onTap: () {
          showAerisCardPage(
            context,
            (context) => PipelineDetailPage(pipeline: widget.pipeline),
          ).then((_) => setState(() {}));
        },
        color: widget.pipeline.enabled == false
            ? const Color.fromARGB(115, 34, 34, 34).withOpacity(0.8)
            : null,
        body: Container(
            width: double.infinity,
            padding:
                const EdgeInsets.only(top: 20, bottom: 20, left: 30, right: 10),
            child: Row(children: [
              Expanded(
                  flex: 4,
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    mainAxisAlignment: MainAxisAlignment.start,
                    children: [
                      Text(widget.pipeline.name,
                          style: TextStyle(
                            fontSize: 22,
                            color: Theme.of(context).colorScheme.onSurface,
                          )),
                      const SizedBox(height: 10),
                      Text(widget.pipeline.trigger.lastToString(),
                          style: TextStyle(
                              color: widget.pipeline.enabled == false
                                  ? Theme.of(context).colorScheme.onSurface
                                  : const Color.fromARGB(255, 83, 83, 83),
                              fontSize: 12)),
                    ],
                  )),
              Expanded(
                  flex: 5,
                  child: Row(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        widget.pipeline.trigger.service.getLogo(),
                        const SizedBox(width: 10),
                        Icon(
                          Icons.arrow_forward,
                          color: Theme.of(context).colorScheme.onSurface,
                        ),
                        const SizedBox(width: 10),
                        Column(children: reactionLogos)
                      ])),
              Expanded(
                  flex: 1,
                  child: Column(
                      children: [
                        Icon(
                          Icons.arrow_forward_ios,
                          color: Theme.of(context).colorScheme.onSurface,
                        )
                      ],
                      mainAxisAlignment: MainAxisAlignment.center,
                      crossAxisAlignment: CrossAxisAlignment.start)),
            ])));
  }
}
