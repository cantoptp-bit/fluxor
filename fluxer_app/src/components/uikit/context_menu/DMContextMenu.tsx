/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {DataMenuRenderer} from '@app/components/uikit/context_menu/DataMenuRenderer';
import {useDMMenuData} from '@app/components/uikit/context_menu/items/DMMenuData';
import {MuteDMMenuItem} from '@app/components/uikit/context_menu/items/DMMenuItems';
import {TempChatUserMenuItem} from '@app/components/uikit/context_menu/items/TempChatUserMenuItem';
import {MenuGroup} from '@app/components/uikit/context_menu/MenuGroup';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {UserRecord} from '@app/records/UserRecord';
import RelationshipStore from '@app/stores/RelationshipStore';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface DMContextMenuProps {
	channel: ChannelRecord;
	recipient?: UserRecord | null;
	onClose: () => void;
}

export const DMContextMenu: React.FC<DMContextMenuProps> = observer(({channel, recipient, onClose}) => {
	const {t} = useLingui();

	const {groups} = useDMMenuData(channel, recipient, {
		onClose,
	});

	const excludeLabels = useMemo(() => [t`Mute Conversation`, t`Unmute Conversation`], [t]);

	const is1to1DM = channel.type === ChannelTypes.DM;
	const relationshipType = recipient ? RelationshipStore.getRelationship(recipient.id)?.type : undefined;
	const isFriend = relationshipType === RelationshipTypes.FRIEND && recipient && !recipient.bot;

	return (
		<>
			<DataMenuRenderer groups={groups} excludeLabels={excludeLabels} />

			{recipient && is1to1DM && isFriend && (
				<MenuGroup>
					<TempChatUserMenuItem user={recipient} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<MuteDMMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>
		</>
	);
});
